quality_determinism_cache <- new.env(parent = emptyenv())

#' Evaluate an interchangeable quality rule
#'
#' Extension packages implement an S3 method for their rule class, inheriting
#' from `dr_rule`. Return the same columns as a native call. Unknown states,
#' empty evidence and malformed results never authorize publication.
#' Rule exceptions are retained locally only when requested by [dr_validate()].
#' @param rule A rule from [dr_quality_rule()], [dr_pointblank_checks()], or an extension.
#' @param data Data frame or lazy table.
#' @param ... Adapter-specific options. Native methods accept `keep_agent`.
#' @returns A quality tibble, with one row per evaluated check.
#' @export
#' @examples
#' dr_run_quality(dr_quality_rule("positive", ~ amount > 0),
#'   data.frame(amount = c(10, -1, NA)))
dr_run_quality <- function(rule, data, ...) UseMethod("dr_run_quality")


normalize_quality_engine <- function(engine) {
  rlang::local_error_call(rlang::caller_env())
  engine <- match.arg(engine, c("native", "pointblank"))
  if (engine == "native") "r" else engine
}


quality_formula_units <- function(data, predicate) {
  rlang::local_error_call(rlang::caller_env())
  if (!inherits(predicate, "formula") || length(predicate) != 2L) {
    abort(
      subclass = "dataraft_error_quality",
      "Use a one-sided quality formula, for example ~ amount >= 0."
    )
  }
  if (is_lazy_table(data)) {
    expression <- rlang::as_quosure(predicate)
    units <- dplyr::transmute(dplyr::ungroup(data), .dr_pass = !!expression)
    value <- table_prototype(units)$.dr_pass
  } else {
    value <- rlang::eval_tidy(
      predicate[[2]],
      data,
      env = environment(predicate)
    )
    if (!length(value) %in% c(1L, nrow(data))) {
      abort(
        subclass = "dataraft_error_quality",
        "Quality formulas must return one logical value or one per row."
      )
    }
    units <- NULL
  }
  if (!is.logical(value) || !is.null(dim(value))) {
    abort(
      subclass = "dataraft_error_quality",
      "Quality formulas must return one logical value or one per row."
    )
  }
  if (is.null(units)) {
    units <- tibble::tibble(.dr_pass = rep(value, length.out = nrow(data)))
  }
  units
}


#' @export
dr_run_quality.dr_rule <- function(rule, data, ...) {
  assert_stable_rule(rule)
  key <- digest::digest(rule, algo = "sha256")
  verify <- isTRUE(getOption("dataraft.verify_determinism", TRUE)) ||
    !exists(key, quality_determinism_cache, inherits = FALSE)
  remember <- function() {
    if (!isTRUE(rule$volatile) && !isTRUE(rule$dynamic_reference)) {
      if (length(quality_determinism_cache) >= 1000L) {
        rm(
          list = ls(quality_determinism_cache),
          envir = quality_determinism_cache
        )
      }
      assign(key, TRUE, envir = quality_determinism_cache)
    }
  }
  if (identical(rule$engine, "pointblank")) {
    out <- pointblank_results(rule, data, ...)
    if (verify && !isTRUE(rule$volatile) && !isTRUE(rule$dynamic_reference)) {
      repeated <- pointblank_results(rule, data, ...)
      if (!identical(lapply(out, identity), lapply(repeated, identity))) {
        abort(
          "Quality rule changed on repeated evaluation; declare volatile = TRUE.",
          subclass = "dataraft_error_quality"
        )
      }
    }
    if (isTRUE(rule$volatile)) {
      out$engine <- "pointblank:volatile"
      out$message <- "Volatile rule: outcome is not reusable evidence or an approval."
      out$status[out$status == "passed"] <- "warning"
    }
    remember()
    return(out)
  }
  if (!identical(rule$engine, "r")) {
    abort(
      subclass = "dataraft_error_quality",
      "This quality engine needs a dr_run_quality() method."
    )
  }
  if (inherits(rule$check, "formula")) {
    units <- quality_formula_units(data, rule$check)
    if (verify && !isTRUE(rule$volatile)) {
      repeated <- quality_formula_units(data, rule$check)
      if (!identical(dr_collect(units), dr_collect(repeated))) {
        abort(
          "Quality rule changed on repeated evaluation; declare volatile = TRUE.",
          subclass = "dataraft_error_quality"
        )
      }
    }
    if (is_lazy_table(units)) {
      .dr_pass <- NULL
      counts <- dplyr::collect(dplyr::summarise(
        units,
        failed = sum(as.integer(is.na(.dr_pass) | !.dr_pass), na.rm = TRUE),
        total = dplyr::n()
      ))
      value <- dr_quality_counts(
        if (is.na(counts$failed)) 0 else counts$failed,
        counts$total
      )
    } else {
      value <- units$.dr_pass
      value <- dr_quality_counts(sum(is.na(value) | !value), length(value))
    }
  } else {
    value <- rule$check(data)
    if (
      verify &&
        !isTRUE(rule$volatile) &&
        !isTRUE(rule$dynamic_reference) &&
        !identical(value, rule$check(data))
    ) {
      abort(
        "Quality rule changed on repeated evaluation; declare volatile = TRUE.",
        subclass = "dataraft_error_quality"
      )
    }
    if (is.logical(value) && is.null(dim(value))) {
      if (length(value) != 1L && length(value) != count_rows(data)) {
        abort(
          subclass = "dataraft_error_quality",
          "A quality function must return one logical value, one per row, or dr_quality_counts()."
        )
      }
      value <- dr_quality_counts(sum(is.na(value) | !value), length(value))
    }
  }
  if (inherits(value, "dr_quality_counts")) {
    out <- from_counts(
      rule$name,
      value$n_failed,
      value$n_total,
      if (identical(rule$action, "warn")) "warning" else "error",
      if (identical(rule$action, "quarantine")) 0 else rule$threshold
    )
  } else {
    abort(
      subclass = "dataraft_error_quality",
      "A quality function must return logical values or dr_quality_counts(); numeric scores are not pass/fail results."
    )
  }
  out$engine <- "r"
  if (isTRUE(rule$volatile)) {
    out$engine <- "r:volatile"
    out$message <- "Volatile rule: outcome is not reusable evidence or an approval."
    if (out$status == "passed") out$status <- "warning"
  }
  remember()
  out
}


#' @export
dr_run_quality.default <- function(rule, data, ...) {
  abort(
    subclass = "dataraft_error_quality",
    "Use dr_quality_rule(), dr_pointblank_checks(), or a rule with a dr_run_quality() method."
  )
}


check_quality_output <- function(rows) {
  rlang::local_error_call(rlang::caller_env())
  columns <- names(quality_row("", ""))
  if (!is.data.frame(rows) || !all(columns %in% names(rows)) || !nrow(rows)) {
    abort(
      subclass = "dataraft_error_quality",
      "A quality adapter must return a non-empty quality tibble with the documented columns."
    )
  }
  if (
    anyNA(rows$status) ||
      !all(
        rows$status %in%
          c("passed", "warning", "failed", "error", "not_checked")
      ) ||
      anyNA(rows$rule) ||
      any(!nzchar(rows$rule)) ||
      anyNA(rows$severity) ||
      !all(rows$severity %in% c("error", "warning"))
  ) {
    abort(
      subclass = "dataraft_error_quality",
      "The quality adapter returned invalid rule names, states or severities."
    )
  }
  if (!is.numeric(rows$n_failed) || !is.numeric(rows$n_total)) {
    abort(
      subclass = "dataraft_error_quality",
      "Quality counts must be numeric."
    )
  }
  evaluated <- rows$status %in% c("passed", "warning", "failed")
  valid <- is.finite(rows$n_failed) &
    is.finite(rows$n_total) &
    rows$n_total > 0 &
    rows$n_failed >= 0 &
    rows$n_failed <= rows$n_total
  if (any(evaluated & !valid)) {
    abort(
      subclass = "dataraft_error_quality",
      "Evaluated quality rules need valid counts and at least one test unit."
    )
  }
  rows[columns]
}


evaluate_rules <- function(
  rules,
  data,
  keep_agents = FALSE,
  keep_errors = FALSE
) {
  rlang::local_error_call(rlang::caller_env())
  agents <- errors <- list()
  rows <- lapply(rules, function(rule) {
    tryCatch(
      {
        result <- dr_run_quality(rule, data, keep_agent = keep_agents)
        if (keep_agents) {
          agents[[rule$name]] <<- attr(result, "pointblank_agent")
        }
        check_quality_output(result)
      },
      error = function(e) {
        if (keep_errors) {
          errors[[rule$name]] <<- e
        }
        quality_row(
          rule$name,
          "error",
          if (identical(rule$action, "warn")) "warning" else "error",
          message = "Rule execution failed. Inspect locally retained conditions with dr_quality_errors(result). Raw exception text is not exported.",
          engine = rule$engine %||% "custom"
        )
      }
    )
  })
  out <- if (length(rows)) dplyr::bind_rows(rows) else quality_row("", "")[0, ]
  if (keep_agents) {
    attr(out, "pointblank_agents") <- agents
  }
  if (keep_errors) {
    attr(out, "dr_errors") <- errors
  }
  out
}


assert_stable_rule <- function(rule) {
  if (isTRUE(rule$volatile)) {
    return(invisible(NULL))
  }
  unstable <- c(
    "runif",
    "rnorm",
    "sample",
    "sample.int",
    "Sys.time",
    "Sys.Date",
    "date",
    "proc.time",
    "RANDOM",
    "random",
    "uuid",
    "UUIDgenerate"
  )
  inspect <- function(check, seen = list(), depth = 0L) {
    if (depth > 16L || any(vapply(seen, identical, logical(1), y = check))) {
      return(character())
    }
    expr <- if (inherits(check, "formula")) check[[2]] else body(check)
    probe <- function() NULL
    body(probe) <- expr
    environment(probe) <- environment(check)
    calls <- codetools::findGlobals(probe, merge = FALSE)$functions
    qualified <- function(expr) {
      if (missing(expr) || !is.call(expr)) {
        return(character())
      }
      out <- if (
        identical(expr[[1L]], as.name("::")) ||
          identical(expr[[1L]], as.name(":::"))
      ) {
        as.character(expr[[3L]])
      } else {
        character()
      }
      unique(c(out, unlist(lapply(as.list(expr), qualified))))
    }
    calls <- union(calls, qualified(expr))
    found <- intersect(calls, unstable)
    env <- environment(check)
    for (name in setdiff(unique(calls), c(unstable, "{", "(", "::", ":::"))) {
      binding <- tryCatch(
        get(name, envir = env, inherits = TRUE),
        error = function(e) NULL
      )
      if (
        is.function(binding) &&
          !is.primitive(binding) &&
          !isNamespace(environment(binding)) &&
          !identical(environment(binding), baseenv())
      ) {
        found <- union(
          found,
          inspect(binding, c(seen, list(check)), depth + 1L)
        )
      }
    }
    found
  }
  found <- inspect(rule$check)
  if (length(found)) {
    abort(
      paste0(
        "Nondeterministic quality rule (",
        paste(found, collapse = ", "),
        "). Declare volatile = TRUE or use fixed inputs."
      ),
      subclass = "dataraft_error_quality"
    )
  }
  invisible(NULL)
}
