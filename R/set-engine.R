#' Choose an implementation without changing a specification
#'
#' Mirrors parsnip's separation of intent and implementation. Formula quality
#' rules support `"native"` and `"pointblank"`; checked lookup specifications
#' support `"native"` and `"dm"`. Engine selection never reads data or loads an
#' optional engine. Missing dependencies are reported at execution preflight.
#' These engines implement individual operations, not the whole data platform.
#' Storage is configured separately with [dr_set_target()] or `dr_publish(to = )`.
#' @param x A [dr_quality_rule()] or [dr_lookup_spec()] specification.
#' @param engine Supported engine name.
#' @param ... Reserved for extension methods. Built-in methods reject extras.
#' @returns An updated specification. The original is unchanged.
#' @export
#' @examples
#' positive <- dr_quality_rule("positive", ~ amount > 0)
#' positive |> dr_set_engine("pointblank")
dr_set_engine <- function(x, engine, ...) UseMethod("dr_set_engine")


#' @export
dr_set_engine.default <- function(x, engine, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "Use dr_set_engine() with a dr_quality_rule() or dr_lookup_spec()."
  )
}


#' @export
dr_set_engine.dr_rule <- function(x, engine, ...) {
  rlang::check_dots_empty()
  if (!identical(class(x), "dr_rule")) {
    abort(
      subclass = "dataraft_error_definition",
      "This quality extension needs its own dr_set_engine() method."
    )
  }
  args <- x[c("name", "check", "severity", "max_failure", "description")]
  args$engine <- engine
  do.call(dr_quality_rule, args)
}


#' @export
dr_set_engine.dr_lookup_transform <- function(x, engine, ...) {
  rlang::check_dots_empty()
  scalar(engine, "engine")
  x$engine <- match.arg(engine, c("native", "dm"))
  x$engine_explicit <- TRUE
  x
}


#' Specify a reusable checked relationship
#'
#' Defines an enrichment independently of a product or recipe. Use [dr_set_engine()]
#' to choose native or dm constraint checks, then attach it with
#' [dr_step_transform()].
#' @param source Reference data, path, source adapter, product or successful result.
#' @param by Equality keys as a character vector, named vector or [dplyr::join_by()].
#' @param unmatched Whether unmatched input rows fail or retain missing attributes.
#' @param suffix Two suffixes for overlapping non-key column names.
#' @param name Stable reference delivery name for source replacement.
#' @param table Member table to select from a successful model result.
#' @returns A deferred lookup transformation specification.
#' @export
#' @examples
#' customers <- data.frame(id = 1:2, region = c("North", "South"))
#' lookup <- dr_lookup_spec(customers, by = "id", name = "customers") |>
#'   dr_set_engine("native")
#' dr_recipe() |> dr_step_transform(lookup)
dr_lookup_spec <- function(
  source,
  by,
  unmatched = c("error", "keep"),
  suffix = c(".x", ".y"),
  name = NULL,
  table = NULL
) {
  if (is.null(name)) {
    name <- if (!is.null(table)) {
      table
    } else if (inherits(source, "dr_product")) {
      source$id
    } else if (is.symbol(substitute(source))) {
      as.character(substitute(source))
    }
  }
  holder <- dr_add_lookup(
    dr_product("lookup"),
    source,
    by,
    unmatched = match.arg(unmatched),
    suffix = suffix,
    name = name,
    table = table
  )
  holder$transforms[[1L]]
}


#' @export
print.dr_rule <- function(x, ...) {
  cat("<quality rule> ", x$name, "\n", sep = "")
  cat("Engine:", if (identical(x$engine, "r")) "native" else x$engine, "\n")
  cat(
    "Severity:",
    x$severity,
    "| Allowed failure proportion:",
    x$max_failure,
    "\n"
  )
  invisible(x)
}


#' @export
print.dr_lookup_transform <- function(x, ...) {
  cat("<lookup specification> ", x$name, "\n", sep = "")
  cat("Engine:", x$engine, "| Unmatched:", x$unmatched, "\n")
  cat("Keys:", paste(names(x$by), x$by, sep = " = ", collapse = ", "), "\n")
  invisible(x)
}
