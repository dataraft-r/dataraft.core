#' Enrich a product through a checked many-to-one lookup
#'
#' Add columns from a reference table while preserving the input row grain.
#' The reference keys must be unique and non-missing. By default, every input
#' key must match, including every component of a composite key. Missing input
#' keys never match. With `unmatched = "keep"`, unmatched input rows remain
#' and receive missing reference attributes.
#'
#' dm constructs and examines primary and foreign key constraints. The join
#' uses dplyr and verifies the row count. Only the dm engine is supported.
#' Lookup checks run at this transformation step, before subsequent transforms.
#' Ordinary [dr_add_quality()] checks still apply to the final candidate.
#'
#' The reference is an explicit execution dependency: products are validated
#' and shared results are reused within a run. No source is read at definition
#' time. Tables on the same database remain lazy; checks collect counts and,
#' for dm, at most one diagnostic example per violated constraint. Those key
#' values are not included in execution metadata. Cross-backend joins require
#' the caller to collect or copy tables
#' explicitly before using this step. This helper does not perform that transfer.
#' @param x A product specification.
#' @param source Reference data, a path, source adapter, product, or successful
#'   result. Lake results pin exact immutable releases; other results reuse
#'   their retained output.
#' @param by Equality keys, supplied as a character vector, a named vector
#'   mapping input to reference columns, or [dplyr::join_by()]. Inequality,
#'   rolling and cross joins belong in ordinary dplyr transformations.
#' @param engine Constraint validation engine: `"dm"` only.
#' @param unmatched Whether unmatched input rows cause an error or are retained
#'   with missing reference values. Unused reference rows are always allowed.
#' @param suffix Two suffixes for overlapping non-key column names, as in
#'   [dplyr::left_join()].
#' @param name Stable delivery name used by `sources = list(name = new_data)`.
#'   Defaults to the reference product id or a bare source variable's name.
#'   For expressions such as file readers, supply a name explicitly; otherwise
#'   an automatic lookup name is used. [dr_plan()] shows the available names.
#' @param table Table to select when source is a successful model result. The
#'   table name is also the default delivery name; published members stay pinned.
#' @returns An updated product specification.

#' @examples
#' customers <- data.frame(id = c("a", "b"), region = c("North", "South"))
#' dr_product("orders") |>
#'   dr_add_source(data.frame(customer_id = c("a", "a", "b"))) |>
#'   dr_add_lookup(customers, by = c(customer_id = "id")) |>
#'   dr_run() |>
#'   dr_collect()
#' @keywords internal
#' @noRd
dr_add_lookup <- function(
  x,
  source,
  by,
  engine = "dm",
  unmatched = c("error", "keep"),
  suffix = c(".x", ".y"),
  name = NULL,
  table = NULL
) {
  source_expr <- substitute(source)
  if (!is.null(table)) {
    source <- model_member_result(source, table)
    name <- name %||% table
  } else if (inherits(source, "dr_model_result")) {
    abort(
      subclass = "dataraft_error_definition",
      "Choose the model lookup table with table = 'table_name'."
    )
  }
  x <- editable_product(x)
  step_name <- paste0("lookup_", length(x$transforms) + 1L)
  name <- name %||%
    if (inherits(source, "dr_product")) {
      source$id
    } else if (is.symbol(source_expr)) {
      as.character(source_expr)
    } else {
      step_name
    }
  scalar(name, "name")
  aliases <- delivery_aliases(x)
  existing <- if (name %in% names(aliases)) {
    product_sources(x)[[aliases[[name]]]]
  } else {
    NULL
  }
  if (
    name %in% names(aliases) && !same_delivery_product(existing, source, name)
  ) {
    abort(
      subclass = "dataraft_error_definition",
      paste0(
        "Delivery name '",
        name,
        "' is already used. Supply a unique name in dr_add_lookup(name = )."
      )
    )
  }
  step <- structure(
    list(
      name = name,
      source = normalize_source(source, id = x$id, name = name),
      by = lookup_keys(by),
      engine = match.arg(engine, "dm"),
      engine_explicit = !missing(engine),
      unmatched = match.arg(unmatched),
      suffix = lookup_suffix(suffix)
    ),
    class = "dr_lookup_transform"
  )
  dr_add_transform(x, step, name = step_name)
}


lookup_keys <- function(by) {
  rlang::local_error_call(rlang::caller_env())
  if (inherits(by, "dplyr_join_by")) {
    if (
      !length(by$x) || any(by$condition != "==") || any(by$filter != "none")
    ) {
      abort(
        subclass = "dataraft_error_definition",
        "A checked lookup needs equality keys. Use an ordinary dplyr join for other relationships.",
        "dr_lookup_invalid"
      )
    }
    by <- stats::setNames(by$y, by$x)
  }
  if (!is.character(by) || !length(by) || anyNA(by) || any(!nzchar(by))) {
    abort(
      subclass = "dataraft_error_definition",
      "by must name one or more equality keys.",
      "dr_lookup_invalid"
    )
  }
  if (is.null(names(by))) {
    names(by) <- by
  }
  if (!anyNA(names(by))) {
    names(by)[!nzchar(names(by))] <- by[!nzchar(names(by))]
  }
  if (
    anyNA(names(by)) ||
      any(!nzchar(names(by))) ||
      anyDuplicated(names(by)) ||
      anyDuplicated(unname(by))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "by must map unique input keys to unique reference keys.",
      "dr_lookup_invalid"
    )
  }
  by
}


lookup_suffix <- function(suffix) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.character(suffix) || length(suffix) != 2L || anyNA(suffix)) {
    abort(
      subclass = "dataraft_error_definition",
      "suffix must contain two character strings.",
      "dr_lookup_invalid"
    )
  }
  suffix
}


#' @export
component_sources.dr_lookup_transform <- function(x, ...) {
  rlang::local_error_call(rlang::caller_env())
  list(lookup = x$source)
}


#' @export
replace_component_sources.dr_lookup_transform <- function(x, sources, ...) {
  rlang::local_error_call(rlang::caller_env())
  x$source <- sources$lookup
  x
}


#' @export
dr_check_component.dr_lookup_transform <- function(x, ...) {
  lookup_keys(x$by)
  lookup_suffix(x$suffix)
  match.arg(x$engine, "dm")
  match.arg(x$unmatched, c("error", "keep"))
  if (x$engine == "dm") {
    need("dm")
  }
  invisible(x)
}


#' @export
dr_inspect.dr_lookup_transform <- function(x, ...) {
  source <- if (inherits(x$source, "dr_product")) {
    list(type = "product", id = x$source$id, version = x$source$version)
  } else {
    dr_inspect(x$source)
  }
  source$rows <- NULL
  if (is.list(source$data)) {
    source$data$rows <- NULL
  }
  list(
    type = "checked lookup",
    engine = x$engine,
    by = x$by,
    unmatched = x$unmatched,
    suffix = x$suffix,
    source = source
  )
}


#' @export
dr_execute_transform.dr_lookup_transform <- function(
  transform,
  data,
  ...,
  sources = list()
) {
  dr_check_component(transform)
  reference <- sources$lookup
  if (is.null(reference)) {
    abort(
      subclass = "dataraft_error_definition",
      "A lookup reference must be resolved by dr_run().",
      "dr_lookup_invalid"
    )
  }
  data <- table_result(data, "Lookup input")
  reference <- table_result(reference, "Lookup reference")
  by <- transform$by
  input_keys <- names(by)
  parent_keys <- unname(by)
  if (
    !all(input_keys %in% names(table_prototype(data))) ||
      !all(parent_keys %in% names(table_prototype(reference)))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Lookup key columns are missing from the input or reference table.",
      "dr_lookup_invalid"
    )
  }
  same <- tryCatch(dplyr::same_src(data, reference), error = \(e) FALSE)
  if (!isTRUE(same)) {
    abort(
      subclass = "dataraft_error_definition",
      "Lookup tables are on different backends. Collect or copy them explicitly onto the same backend before joining.",
      "dr_lookup_backend"
    )
  }
  left <- dplyr::ungroup(data)
  right <- dplyr::ungroup(reference)
  dm_checks <- lookup_dm_constraints(left, right, by, transform$unmatched)
  joined <- dplyr::left_join(
    data,
    right,
    by = by,
    suffix = transform$suffix,
    na_matches = "never"
  )
  input_rows <- count_rows(left)
  output_rows <- count_rows(joined)
  if (output_rows != input_rows) {
    abort(
      subclass = "dataraft_error_definition",
      "The lookup changed the number of input rows. Check key types and concurrent changes to the reference table.",
      "dr_lookup_cardinality"
    )
  }
  evidence <- list(
    type = "checked lookup",
    engine = transform$engine,
    keys = by,
    unmatched = transform$unmatched,
    input_rows = input_rows,
    output_rows = output_rows,
    row_preserved = TRUE,
    constraints = list(
      parent_unique = "passed",
      parent_nonmissing = "passed",
      input_keys = if (transform$unmatched == "error") {
        "passed"
      } else {
        "not_required"
      }
    )
  )
  if (!is.null(dm_checks)) {
    evidence$constraints$dm <- as.list(stats::setNames(
      dm_checks$is_key,
      dm_checks$kind
    ))
  }
  attr(joined, "dr_transform_metadata") <- evidence
  joined
}


lookup_missing_keys <- function(data, keys) {
  rlang::local_error_call(rlang::caller_env())
  missing <- dplyr::filter(data, dplyr::if_any(dplyr::all_of(keys), is.na))
  count_rows(missing)
}


lookup_parent_error <- function() {
  rlang::local_error_call(rlang::caller_env())
  abort(
    subclass = "dataraft_error_definition",
    "Lookup reference keys must be unique and non-missing. Fix the reference table before enriching the input.",
    "dr_lookup_parent_key"
  )
}


lookup_unmatched_error <- function(data = NULL) {
  rlang::local_error_call(rlang::caller_env())
  abort(
    subclass = "dataraft_error_definition",
    "Some input lookup keys are missing or have no reference match. Correct the keys, or use unmatched = 'keep' to retain missing attributes explicitly.",
    "dr_lookup_unmatched",
    diagnostic = if (!is.null(data)) {
      list(data = data, failed_rows = TRUE)
    } else {
      NULL
    }
  )
}


lookup_dm_constraints <- function(data, reference, by, unmatched) {
  rlang::local_error_call(rlang::caller_env())
  need("dm")
  model <- dm::dm(input = data, reference = reference) |>
    dm::dm_add_pk(reference, dplyr::all_of(!!unname(by))) |>
    dm::dm_add_fk(
      !!rlang::sym("input"),
      dplyr::all_of(!!names(by)),
      reference,
      dplyr::all_of(!!unname(by))
    )
  checks <- dm::dm_examine_constraints(model, .max_value = 1L)
  if (sum(checks$kind == "PK") != 1L || sum(checks$kind == "FK") != 1L) {
    abort(
      subclass = "dataraft_error_definition",
      "The dm lookup did not return all declared constraint checks.",
      "dr_lookup_constraints"
    )
  }
  if (
    !isTRUE(all(checks$is_key[checks$kind == "PK"])) ||
      lookup_missing_keys(reference, unname(by)) > 0
  ) {
    lookup_parent_error()
  }
  if (
    unmatched == "error" &&
      (!all(checks$is_key[checks$kind == "FK"]) ||
        lookup_missing_keys(data, names(by)) > 0)
  ) {
    lookup_unmatched_error(dplyr::anti_join(
      data,
      reference,
      by = by,
      na_matches = "never"
    ))
  }
  invisible(checks)
}
