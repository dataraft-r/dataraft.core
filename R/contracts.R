#' Define the shape and identity of a data contract
#'
#' Declare columns and keys here. Add business metadata with [dr_contract_meta()]
#' and validation policy with [dr_contract_policy()]. Attach to a product with
#' [dr_add_contract()]. Construction does not read data.
#' @param id Contract identifier; omitted identifiers are scoped to their product.
#' @param columns Named R type vector, or named list of zero-length prototypes.
#' @param key Unique, non-null key columns.
#' @param rules Quality rules or named row predicates.
#' @param version Immutable definition version.
#' @param ... Deprecated legacy policy/metadata arguments. Use the composition
#'   functions instead; this compatibility path is removed on 2027-01-01.
#' @returns A serializable contract specification.
#' @export
#' @examples
#' dr_contract("orders", c(id = "integer", amount = "numeric"), key = "id") |>
#'   dr_contract_meta(owner = "Analytics") |>
#'   dr_contract_policy(allow_extra = TRUE)
dr_contract <- function(
  id = "contract",
  columns,
  key = character(),
  rules = list(),
  version = "1.0.0",
  ...
) {
  anonymous <- missing(id)
  legacy <- list(...)
  if (length(legacy)) {
    lifecycle::deprecate_soft(
      "0.1.0.9005",
      "dr_contract(...)",
      details = "Use dr_contract_meta() and dr_contract_policy() for metadata and policy."
    )
  }
  out <- do.call(
    new_contract,
    c(
      list(
        id = id,
        columns = columns,
        key = key,
        rules = rules,
        version = version
      ),
      legacy
    )
  )
  attr(out, "dr_anonymous") <- if (anonymous) TRUE else NULL
  out
}

new_contract <- function(
  id = "contract",
  version = "1.0.0",
  owner = "",
  description = "",
  grain = "",
  columns,
  required = names(columns),
  key = character(),
  rules = list(),
  producer = owner,
  max_age_hours = NULL,
  allow_empty = FALSE,
  allow_extra = FALSE,
  operator = NULL,
  column_metadata = list(),
  governance = list(),
  constraints = list()
) {
  rlang::local_error_call(rlang::caller_env())
  anonymous <- missing(id)
  if (
    !is.list(governance) ||
      (length(governance) &&
        (is.null(names(governance)) ||
          anyDuplicated(names(governance)) ||
          any(!nzchar(names(governance)))))
  ) {
    abort(
      "governance must be a named list.",
      subclass = "dataraft_error_contract"
    )
  }
  if (is.list(columns)) {
    if (any(lengths(columns) != 0L)) {
      abort(
        subclass = "dataraft_error_contract",
        "Prototype columns must be zero-length R vectors, for example integer()."
      )
    }
    columns <- infer_column_types(tibble::as_tibble(
      columns,
      .name_repair = "minimal"
    ))
  }
  asset_id(id)
  scalar(version, "version")
  for (field in c("owner", "description", "grain", "producer")) {
    value <- get(field)
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      abort(
        subclass = "dataraft_error_contract",
        paste(field, "must be a string; use an empty string to omit it.")
      )
    }
  }
  flag(allow_empty, "allow_empty")
  flag(allow_extra, "allow_extra")
  if (!is.null(operator)) {
    scalar(operator, "operator")
  }
  if (
    is.null(names(columns)) || anyDuplicated(names(columns)) || !length(columns)
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "columns must be a named type vector."
    )
  }
  invisible(lapply(names(columns), column_name))
  if (
    !all(
      columns %in%
        c(
          "character",
          "integer",
          "numeric",
          "logical",
          "Date",
          "POSIXct",
          "integer64",
          "list"
        )
    )
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "Unsupported contract column type.",
      columns = columns
    )
  }
  if (!all(c(required, key) %in% names(columns))) {
    abort(
      subclass = "dataraft_error_contract",
      "required and key must refer to declared columns.",
      columns = setdiff(c(required, key), names(columns))
    )
  }
  if (
    !is.null(max_age_hours) &&
      (!is.numeric(max_age_hours) ||
        length(max_age_hours) != 1 ||
        is.na(max_age_hours) ||
        !is.finite(max_age_hours) ||
        max_age_hours <= 0)
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "max_age_hours must be positive and finite, or NULL."
    )
  }
  constrained <- contract_constraint_rules(constraints, columns, required, key)
  required <- constrained$required
  rules <- c(normalize_quality_rules(rules), constrained$rules)
  if (anyDuplicated(vapply(rules, `[[`, character(1), "name"))) {
    abort(subclass = "dataraft_error_contract", "Rule names must be unique.")
  }
  if (
    length(column_metadata) &&
      (is.null(names(column_metadata)) ||
        anyDuplicated(names(column_metadata)) ||
        !all(names(column_metadata) %in% names(columns)) ||
        !all(vapply(column_metadata, is.list, logical(1))))
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "column_metadata must be named lists for declared columns."
    )
  }
  contract <- structure(
    list(
      id = id,
      version = version,
      kind = "contract",
      owner = owner,
      description = description,
      grain = grain,
      columns = as.list(columns),
      constraints = constraints,
      required = required,
      key = key,
      rules = rules,
      producer = producer,
      max_age_hours = max_age_hours,
      allow_empty = allow_empty,
      allow_extra = allow_extra
    ),
    class = "dr_contract"
  )
  if (!is.null(operator)) {
    contract$operator <- operator
  }
  if (length(column_metadata)) {
    contract$column_metadata <- column_metadata
  }
  if (length(governance)) {
    contract$governance <- governance
  }
  if (anonymous) {
    attr(contract, "dr_anonymous") <- TRUE
  }
  contract
}


#' Define a quality rule
#' @param name Rule name, or a one-sided formula as a shortcut. When omitted,
#'   formula rules use their expression as a label. Explicit names stay unchanged.
#' @param check Function taking a table and returning a logical vector or
#'   dr_quality_counts(), or a one-sided row predicate such as `~ amount >= 0`.
#'   A scalar function result is one aggregate test; a longer vector must have
#'   one value per row. Formula results are evaluated per row, including scalar
#'   predicates repeated for each row. Missing logical values count as failures.
#'   Lazy formulas run on the backend; arbitrary functions remain responsible
#'   for their own collection.
#' @param action Optional publication policy: block, warn or quarantine. Quarantine
#'   removes every failing row before writing and retains it locally on the result.
#'   It supports native row formulas only. Standalone validation still blocks
#'   rejected rows until an execution path actually removes them.
#' @param threshold Permitted fraction of failed test units, from zero to one. For
#'   quarantine, every rejected row is removed regardless of threshold.
#' @param volatile Explicitly allow time-dependent or random checks. Such checks
#'   are labelled volatile, cannot authorize attestations and cannot use release caching.
#' @param dimension Optional ODCS quality dimension.
#' @param severity Compatibility argument: `"error"` corresponds to
#'   `action = "block"`, `"warning"` to `action = "warn"`. Prefer `action`
#'   for new `dr_quality_rule()` definitions. Supplying both is an error.
#'   Deprecated since 0.1.0.9005; use `action` for every engine.
#' @param max_failure Compatibility argument for `threshold`. Prefer `threshold`
#'   for new `dr_quality_rule()` definitions. Supplying both is an error.
#'   Deprecated since 0.1.0.9005. Both names denote a fraction, never a row count.
#' @param description Rule description.
#' @param engine Formula evaluation engine: `"native"` (default) or optional
#'   `"pointblank"`. Both require logical row predicates and count missing
#'   values as failures. Pointblank interrogates a real agent against the
#'   normalized predicate, retaining its reports and check evidence. Ordinary
#'   functions use the native engine; use [dr_pointblank_checks()] for custom agents.
#' @param build Function creating a pointblank agent from a lazy table.
#' @param policy `"rule"` preserves the explicit `action` / `threshold`
#'   gate. `"agent"` uses pointblank's per-step action levels: warnings permit
#'   publication, stop/error and critical states block. Native pointblank
#'   threshold rounding applies. An unconfigured, inactive or errored agent
#'   step always blocks. Old notify-only levels do not authorize publication.
#' @param n_failed,n_total Failed and total test units.
#' @return A rule specification or counts object.
#' @export
#' @examples
#' rule <- dr_quality_rule("positive", function(data) {
#'   counts <- dplyr::summarise(data, failed = sum(amount <= 0), total = dplyr::n())
#'   if (inherits(counts, "tbl_sql")) counts <- dplyr::collect(counts)
#'   dr_quality_counts(counts$failed, counts$total)
#' })
#' rule$name
dr_quality_rule <- function(
  name = NULL,
  check = NULL,
  severity = lifecycle::deprecated(),
  max_failure = lifecycle::deprecated(),
  description = "",
  engine = c("native", "pointblank"),
  action = NULL,
  threshold = NULL,
  dimension = NULL,
  volatile = FALSE
) {
  flag(volatile, "volatile")
  if (lifecycle::is_present(severity)) {
    if (!is.null(action)) {
      abort("Use action or severity, not both.")
    }
    lifecycle::deprecate_soft(
      "0.1.0.9005",
      "dr_quality_rule(severity)",
      "dr_quality_rule(action)"
    )
    action <- if (match.arg(severity, c("error", "warning")) == "warning") {
      "warn"
    } else {
      "block"
    }
  }
  if (lifecycle::is_present(max_failure)) {
    if (!is.null(threshold)) {
      abort("Use threshold or max_failure, not both.")
    }
    lifecycle::deprecate_soft(
      "0.1.0.9005",
      "dr_quality_rule(max_failure)",
      "dr_quality_rule(threshold)"
    )
    threshold <- max_failure
  }
  action <- match.arg(action %||% "block", c("block", "warn", "quarantine"))
  threshold <- threshold %||% 0
  if (!is.null(dimension)) {
    dimension <- match.arg(
      dimension,
      c(
        "accuracy",
        "completeness",
        "conformity",
        "consistency",
        "coverage",
        "timeliness",
        "uniqueness"
      )
    )
  }
  engine_explicit <- !missing(engine)
  if (inherits(name, "formula") && is.null(check)) {
    check <- name
    name <- NULL
  }
  if (is.null(name) && inherits(check, "formula") && length(check) == 2L) {
    name <- rlang::as_label(rlang::f_rhs(check))
  }
  scalar(name, "name")
  if (!is.function(check) && !inherits(check, "formula")) {
    abort(
      subclass = "dataraft_error_contract",
      "check must be a function or a one-sided formula."
    )
  }
  if (inherits(check, "formula") && length(check) != 2L) {
    abort(
      subclass = "dataraft_error_contract",
      "Use a one-sided quality formula, for example ~ amount >= 0."
    )
  }
  engine <- normalize_quality_engine(match.arg(engine))
  if (engine == "pointblank" && !inherits(check, "formula")) {
    abort(
      subclass = "dataraft_error_contract",
      "Pointblank formula rules need a one-sided formula. Use dr_pointblank_checks() for an agent builder."
    )
  }
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1 ||
      !is.finite(threshold) ||
      threshold < 0 ||
      threshold > 1
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "threshold must be between 0 and 1."
    )
  }
  structure(
    list(
      name = name,
      check = check,
      threshold = threshold,
      description = description,
      engine = engine,
      engine_explicit = engine_explicit,
      action = action,
      dimension = dimension,
      volatile = volatile
    ),
    class = "dr_rule"
  )
}

#' @rdname dr_quality_rule
#' @export
dr_quality_counts <- function(n_failed, n_total) {
  if (
    !is.numeric(n_failed) ||
      !is.numeric(n_total) ||
      length(n_failed) != 1 ||
      length(n_total) != 1 ||
      any(!is.finite(c(n_failed, n_total))) ||
      n_failed != trunc(n_failed) ||
      n_total != trunc(n_total) ||
      n_failed < 0 ||
      n_total < n_failed
  ) {
    abort(subclass = "dataraft_error_contract", "Invalid quality counts.")
  }
  structure(
    list(n_failed = n_failed, n_total = n_total),
    class = "dr_quality_counts"
  )
}

#' @rdname dr_quality_rule
#' @export
dr_pointblank_checks <- function(
  name,
  build,
  severity = lifecycle::deprecated(),
  max_failure = lifecycle::deprecated(),
  policy = c("rule", "agent"),
  action = NULL,
  threshold = NULL,
  volatile = FALSE
) {
  args <- list(
    name = name,
    check = build,
    action = action,
    threshold = threshold,
    volatile = volatile
  )
  if (lifecycle::is_present(severity)) {
    lifecycle::deprecate_soft(
      "0.1.0.9005",
      "dr_pointblank_checks(severity)",
      "dr_pointblank_checks(action)"
    )
    if (!is.null(action)) {
      abort("Use action or severity, not both.")
    }
    args$action <- if (
      match.arg(severity, c("error", "warning")) == "warning"
    ) {
      "warn"
    } else {
      "block"
    }
  }
  if (lifecycle::is_present(max_failure)) {
    lifecycle::deprecate_soft(
      "0.1.0.9005",
      "dr_pointblank_checks(max_failure)",
      "dr_pointblank_checks(threshold)"
    )
    if (!is.null(threshold)) {
      abort("Use threshold or max_failure, not both.")
    }
    args$threshold <- max_failure
  }
  if (identical(args$action, "quarantine")) {
    abort(
      "Pointblank agents support block or warn; quarantine requires a native row formula."
    )
  }
  rule <- do.call(dr_quality_rule, args)
  rule$engine <- "pointblank"
  rule$engine_explicit <- TRUE
  policy <- match.arg(policy)
  if (policy != "rule") {
    rule$policy <- policy
  }
  rule
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name quality_row

quality_row <- function(
  rule,
  status,
  severity = "error",
  n_failed = NA_real_,
  n_total = NA_real_,
  threshold = 0,
  message = "",
  engine = "contract",
  stage = "candidate",
  segment = "",
  details = ""
) {
  rlang::local_error_call(rlang::caller_env())
  tibble::tibble(
    rule = rule,
    status = status,
    severity = severity,
    n_failed = as.numeric(n_failed),
    n_total = as.numeric(n_total),
    threshold = threshold,
    message = message,
    engine = engine,
    stage = stage,
    segment = segment,
    details = details
  )
}


from_counts <- function(
  name,
  failed,
  total,
  severity = "error",
  threshold = 0
) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.finite(total) || total <= 0) {
    return(quality_row(
      name,
      "not_checked",
      severity,
      failed,
      total,
      threshold,
      "No test units."
    ))
  }
  status <- if (failed / total <= threshold) {
    "passed"
  } else if (severity == "warning") {
    "warning"
  } else {
    "failed"
  }
  quality_row(name, status, severity, failed, total, threshold)
}


pointblank_results <- function(rule, data, keep_agent = FALSE) {
  rlang::local_error_call(rlang::caller_env())
  need("pointblank")
  formula <- inherits(rule$check, "formula")
  if (formula) {
    units <- quality_formula_units(data, rule$check)
    if (
      !isTRUE(rule$volatile) &&
        !identical(
          dr_collect(units),
          dr_collect(quality_formula_units(data, rule$check))
        )
    ) {
      abort(
        "Quality rule changed on repeated evaluation; declare volatile = TRUE.",
        subclass = "dataraft_error_quality"
      )
    }
    if (!is.data.frame(units) && !inherits(units, "tbl_sql")) {
      abort(
        subclass = "dataraft_error_contract",
        "Pointblank formula checks need a data frame or DBI table. Collect this table explicitly, or use engine = 'native'."
      )
    }
    agent <- pointblank::create_agent(units) |>
      pointblank::col_vals_equal(
        columns = ".dr_pass",
        value = TRUE,
        na_pass = FALSE,
        label = rule$name
      )
  } else {
    agent <- rule$check(data)
  }
  if (!inherits(agent, "ptblank_agent")) {
    abort(
      subclass = "dataraft_error_contract",
      "pointblank builder must return an agent."
    )
  }
  agent <- pointblank::interrogate(
    agent,
    extract_failed = FALSE,
    extract_tbl_checked = FALSE,
    progress = FALSE
  )
  report <- pointblank::get_agent_report(agent, display_table = FALSE)
  if (!nrow(report)) {
    return(quality_row(
      rule$name,
      "not_checked",
      if (identical(rule$action, "warn")) "warning" else "error",
      message = "Empty pointblank plan.",
      engine = "pointblank"
    ))
  }
  results <- dplyr::bind_rows(lapply(seq_len(nrow(report)), function(i) {
    row <- report[i, ]
    name <- if (formula) rule$name else paste(rule$name, row$i, sep = ":")
    if (!isTRUE(row$active[[1]])) {
      return(quality_row(
        name,
        "not_checked",
        if (identical(rule$action, "warn")) "warning" else "error",
        message = "Inactive pointblank step."
      ))
    }
    if (!identical(as.character(row$eval[[1]]), "OK")) {
      return(quality_row(
        name,
        "error",
        if (identical(rule$action, "warn")) "warning" else "error",
        message = "pointblank evaluation did not complete cleanly."
      ))
    }
    counts <- from_counts(
      name,
      as.numeric(row$units - row$n_pass),
      as.numeric(row$units),
      if (identical(rule$action, "warn")) "warning" else "error",
      rule$threshold
    )
    if (identical(rule$policy, "agent")) {
      warn <- if ("W" %in% names(row)) row$W[[1]] else NA
      blocking <- unlist(
        row[intersect(c("S", "E", "C"), names(row))],
        use.names = FALSE
      )
      counts$threshold <- NA_real_
      if (all(is.na(c(warn, blocking)))) {
        counts$status <- "error"
        counts$message <- "Agent policy requires a warning or blocking action level."
      } else if (!counts$status %in% c("error", "not_checked")) {
        counts$status <- if (any(blocking, na.rm = TRUE)) {
          "failed"
        } else if (isTRUE(warn)) {
          "warning"
        } else {
          "passed"
        }
        counts$severity <- if (counts$status == "warning") {
          "warning"
        } else {
          "error"
        }
      }
    }
    counts
  }))
  steps <- agent$validation_set
  if (nrow(steps) != nrow(results)) {
    abort(
      subclass = "dataraft_error_contract",
      "Unsupported pointblank report layout."
    )
  }
  for (i in seq_len(nrow(results))) {
    if (
      all(c("seg_col", "seg_val") %in% names(steps)) &&
        length(steps$seg_col[[i]]) &&
        !all(is.na(steps$seg_col[[i]]))
    ) {
      results$segment[[i]] <- jencode(list(
        columns = steps$seg_col[[i]],
        values = steps$seg_val[[i]]
      ))
    }
    actions <- steps$actions[[i]]
    levels <- actions[setdiff(names(actions), "fns")]
    details <- list(
      assertion = report$type[[i]],
      columns = report$columns[[i]],
      policy = rule$policy %||% "rule",
      action_levels = levels
    )
    if (formula) {
      details$predicate <- paste(deparse(rule$check[[2]]), collapse = "\n")
    }
    results$details[[i]] <- jencode(details)
  }
  results$engine <- "pointblank"
  if (keep_agent) {
    attr(results, "pointblank_agent") <- agent
  }
  results
}


#' Validate data or check a workflow definition
#'
#' With a table and contract, returns quality evidence. With a composed product
#' or pipeline alone, checks its configuration and returns the definition.
#' Preflight does not evaluate source callbacks or prove data quality.
#' @param data A data frame, lazy table, composed product or pipeline.
#' @param ... Arguments forwarded to a validation method.
#' @param contract Contract definition.
#' @param stage Label stored with each check, such as `"ingest"` or
#'   `"candidate"`.
#' @param keep_agents Retain interrogated pointblank agents as an in-memory
#'   attribute for [dr_pointblank_report()]. Defaults to `FALSE`.
#' @param keep_errors Retain original R conditions in an in-memory `dr_errors`
#'   attribute. They can contain private data and are never persisted in the
#'   registry. Inspect with [dr_quality_errors()]. Defaults to `FALSE`.
#' @return For data, a tibble with one row per check; only passed and warning
#'   permit publication. For a workflow, the validated definition.
#' @export
#' @examples
#' contract <- dr_contract(
#'   "orders", version = "1.0.0",
#'   columns = c(order_id = "integer", amount = "numeric"), key = "order_id"
#' )
#' dr_validate(data.frame(order_id = 1:2, amount = c(25, 75)), contract)
dr_validate <- function(data, contract = NULL, ...) UseMethod("dr_validate")


#' @rdname dr_validate
#' @export
dr_validate.default <- function(
  data,
  contract,
  stage = "candidate",
  keep_agents = FALSE,
  keep_errors = FALSE,
  ...
) {
  rlang::check_dots_empty()
  if (!inherits(contract, "dr_contract")) {
    abort(subclass = "dataraft_error_contract", "contract must be a contract.")
  }
  assert_contract_ready(contract)
  scalar(stage, "stage")
  flag(keep_agents, "keep_agents")
  flag(keep_errors, "keep_errors")
  errors <- list()
  result <- list()
  agents <- list()
  add <- function(x) result[[length(result) + 1L]] <<- x
  protect <- function(name, fn) {
    rlang::local_error_call(rlang::caller_env())
    tryCatch(fn(), error = function(e) {
      if (keep_errors) {
        errors[[name]] <<- e
      }
      quality_row(
        name,
        "error",
        message = "Quality evaluation error; inspect rule locally."
      )
    })
  }
  add(protect("schema", function() {
    actual <- names(table_prototype(data))
    expected <- names(contract$columns)
    good <- all(expected %in% actual) &&
      (contract$allow_extra || setequal(actual, expected))
    quality_row(
      "schema",
      if (good) "passed" else "failed",
      n_failed = as.numeric(!good),
      n_total = 1,
      message = if (good) {
        ""
      } else {
        paste("Expected columns:", paste(expected, collapse = ", "))
      }
    )
  }))
  if (!identical(result[[1]]$status[[1]], "passed")) {
    out <- dplyr::bind_rows(result)
    out$stage <- stage
    if (keep_errors) {
      attr(out, "dr_errors") <- errors
    }
    return(out)
  }
  add(protect("types", function() {
    proto <- table_prototype(data)
    good <- vapply(
      names(contract$columns),
      function(n) {
        x <- proto[[n]]
        t <- contract$columns[[n]]
        contract_type_matches(x, t)
      },
      logical(1)
    )
    quality_row(
      "types",
      if (all(good)) "passed" else "failed",
      n_failed = sum(!good),
      n_total = length(good),
      message = paste(names(good)[!good], collapse = ", ")
    )
  }))
  n <- tryCatch(count_rows(data), error = function(e) NA_real_)
  add(quality_row(
    "nonempty",
    if (is.na(n)) {
      "error"
    } else if (n > 0 || contract$allow_empty) {
      "passed"
    } else {
      "failed"
    },
    n_failed = as.numeric(is.na(n) || (n == 0 && !contract$allow_empty)),
    n_total = 1
  ))
  required <- union(contract$required, contract$key)
  missing <- tryCatch(null_counts(data, required), error = function(e) e)
  for (column in required) {
    add(protect(paste0("not_null:", column), function() {
      if (inherits(missing, "error")) {
        stop(missing)
      }
      k <- missing[[column]]
      quality_row(
        paste0("not_null:", column),
        if (k == 0) "passed" else "failed",
        n_failed = k,
        n_total = n
      )
    }))
  }
  if (length(contract$key)) {
    add(protect("unique_key", function() {
      distinct <- count_rows(dplyr::distinct(
        data,
        !!!rlang::syms(contract$key)
      ))
      quality_row(
        "unique_key",
        if (n == distinct) "passed" else "failed",
        n_failed = n - distinct,
        n_total = n
      )
    }))
  }
  rules <- evaluate_rules(contract$rules, data, keep_agents, keep_errors)
  add(rules)
  if (keep_agents) {
    agents <- attr(rules, "pointblank_agents") %||% list()
  }
  if (keep_errors) {
    errors <- c(errors, attr(rules, "dr_errors"))
  }
  out <- dplyr::bind_rows(result)
  out$stage <- stage
  if (isTRUE(contract$automatic_schema)) {
    inferred <- out$rule %in% c("schema", "types") & out$status == "passed"
    out$status[inferred] <- "unvalidated"
    out$message[
      inferred
    ] <- "No declared contract: schema inferred from this delivery, not independently validated."
  }
  out$engine[
    out$engine == "contract" &
      !out$rule %in%
        c(
          "schema",
          "types",
          "nonempty",
          "unique_key",
          paste0("not_null:", union(contract$required, contract$key))
        )
  ] <- "r"
  if (keep_errors) {
    attr(out, "dr_errors") <- errors
  }
  if (keep_agents) {
    attr(out, "pointblank_agents") <- agents
  }
  out
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name quality_ok

quality_ok <- function(results) {
  rlang::local_error_call(rlang::caller_env())
  if (
    !is.data.frame(results) ||
      !all(c("status", "engine", "rule") %in% names(results))
  ) {
    return(FALSE)
  }
  nrow(results) > 0 &&
    isTRUE(all(
      results$status %in%
        c("passed", "warning") |
        (results$status == "unvalidated" &
          results$engine == "contract" &
          grepl("(^|/)(schema|types)$", results$rule))
    ))
}


#' Inspect locally retained quality exceptions
#' Original conditions are available only in memory, never in exported quality
#' reports. Trial results retain them automatically. Use `conditionMessage()`
#' locally to inspect a missing column or another rule execution error.
#' @param quality Results from `dr_validate(..., keep_errors = TRUE)`, a trial/run
#'   result, or a caught DataRaft condition containing `result`. Model results
#'   prefix each rule with its member table name.
#' @returns A named list of original R conditions, empty when none were retained.
#' @export
#' @examples
#' contract <- dr_contract("example", c(id = "integer"),
#'   rules = list(dr_quality_rule("broken", function(data) stop("Check configuration"))))
#' quality <- dr_validate(data.frame(id = 1L), contract, keep_errors = TRUE)
#' lapply(dr_quality_errors(quality), conditionMessage)
dr_quality_errors <- function(quality) {
  # Bound traversal for malformed custom conditions with cyclic result links.
  errors <- function(x, depth = 0L) {
    if (depth >= 20L) {
      return(list())
    }
    if (inherits(x, "condition")) {
      return(errors(x$result, depth + 1L))
    }
    if (inherits(x, "dr_model_result")) {
      out <- list()
      for (table in names(x$members)) {
        member <- errors(x$members[[table]], depth + 1L)
        names(member) <- paste(table, names(member), sep = "/")
        out <- c(out, member)
      }
      return(out)
    }
    if (inherits(x, "dr_run_result")) {
      out <- errors(x$quality, depth + 1L)
      if (length(out)) {
        return(out)
      }
      return(errors(x$error, depth + 1L))
    }
    attr(x, "dr_errors") %||% list()
  }
  errors(quality)
}


normalize_quality_rules <- function(
  quality,
  name = NULL,
  engine = NULL,
  existing = list()
) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.null(engine)) {
    normalize_quality_engine(engine)
  }
  if (is.list(quality) && !inherits(quality, "dr_rule")) {
    if (!is.null(name)) {
      abort(
        subclass = "dataraft_error_contract",
        "Name individual rules in the quality list."
      )
    }
    for (i in seq_along(quality)) {
      label <- names(quality)[i]
      if (is.null(label) || is.na(label) || !nzchar(label)) {
        label <- NULL
      }
      existing <- normalize_quality_rules(quality[[i]], label, engine, existing)
    }
    return(existing)
  }
  if (!inherits(quality, "dr_rule")) {
    if (is.null(name)) {
      label <- if (inherits(quality, "formula") && length(quality) == 2L) {
        rlang::as_label(rlang::f_rhs(quality))
      } else {
        paste0("quality_", length(existing) + 1L)
      }
      used <- vapply(existing, `[[`, character(1), "name")
      name <- utils::tail(make.unique(c(used, label)), 1L)
    }
    quality <- dr_quality_rule(name, quality)
  } else if (!is.null(name)) {
    quality$name <- scalar(name, "name")
  }
  if (!is.null(engine)) {
    selected <- normalize_quality_engine(engine)
    if (
      !identical(quality$engine, selected) &&
        !inherits(quality$check, "formula")
    ) {
      abort(
        subclass = "dataraft_error_contract",
        "Only formula rules can change engines. Keep custom functions native or use dr_pointblank_checks() for an agent builder."
      )
    }
    quality$engine <- selected
    quality$engine_explicit <- TRUE
  }
  if (quality$name %in% vapply(existing, `[[`, character(1), "name")) {
    abort(
      subclass = "dataraft_error_contract",
      "Quality rule names must be unique."
    )
  }
  c(existing, list(quality))
}
