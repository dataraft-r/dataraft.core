#' Inspect static column dependencies in a recipe
#'
#' Tracks direct field references through mutate, select and rename without
#' executing expressions. Arithmetic, comparisons and a small set of pure base
#' functions are understood. Other verbs, dynamic selectors, joins, custom
#' functions and opaque transforms return complete = FALSE. Incomplete lineage
#' must not be presented as proof that no dependency exists. Row selection and
#' historical execution DAGs are outside this direct-value analysis.
#' @param recipe A recipe definition.
#' @param columns Input column names.
#' @returns A list with fields mapping outputs to original columns, complete,
#'   and reason. An unknown step invalidates the map instead of guessing.
#' @export
dr_column_lineage <- function(recipe, columns) {
  assert_recipe(recipe)
  if (!is.character(columns) || anyNA(columns) || anyDuplicated(columns)) {
    abort("Supply unique input column names.")
  }
  fields <- stats::setNames(lapply(columns, identity), columns)
  unknown <- function(reason) {
    list(fields = list(), complete = FALSE, reason = reason)
  }
  allowed <- c(
    "(",
    "+",
    "-",
    "*",
    "/",
    "^",
    "==",
    "!=",
    ">",
    ">=",
    "<",
    "<=",
    "&",
    "|",
    "!",
    "abs",
    "round",
    "sqrt",
    "log",
    "exp",
    "is.na"
  )
  dependencies <- function(expr) {
    if (is.symbol(expr)) {
      name <- as.character(expr)
      if (!name %in% names(fields)) {
        stop("Unknown input or external binding.")
      }
      return(fields[[name]])
    }
    if (is.atomic(expr) && length(expr) <= 1L) {
      return(character())
    }
    if (
      !is.call(expr) ||
        !is.symbol(expr[[1]]) ||
        !as.character(expr[[1]]) %in% allowed
    ) {
      stop("Opaque expression.")
    }
    unique(unlist(lapply(as.list(expr)[-1], dependencies), use.names = FALSE))
  }
  for (step in recipe$steps) {
    if (
      !inherits(step, "dr_dplyr_transform") ||
        !step$verb %in% c("mutate", "select", "rename")
    ) {
      return(unknown("Unsupported transformation or row-level dependency."))
    }
    args <- step$args
    if (step$verb == "mutate") {
      if (any(names(args) %in% c(".keep", ".before", ".after", ".by"))) {
        return(unknown("Mutation layout or grouping options."))
      }
      if (is.null(names(args)) || any(!nzchar(names(args)))) {
        return(unknown("Unnamed mutation."))
      }
      for (name in names(args)) {
        expression <- rlang::get_expr(args[[name]])
        if (is.null(expression)) {
          fields[name] <- NULL
          next
        }
        value <- tryCatch(dependencies(expression), error = identity)
        if (inherits(value, "error")) {
          return(unknown(conditionMessage(value)))
        }
        fields[name] <- list(value)
      }
    } else {
      selected <- list()
      for (i in seq_along(args)) {
        expr <- rlang::get_expr(args[[i]])
        if (!is.symbol(expr) || !as.character(expr) %in% names(fields)) {
          return(unknown("Dynamic selector or unknown field."))
        }
        old <- as.character(expr)
        name <- names(args)[i]
        if (is.null(name) || is.na(name) || !nzchar(name)) {
          name <- old
        }
        selected[name] <- fields[old]
        if (step$verb == "rename") fields[old] <- NULL
      }
      fields <- if (step$verb == "select") selected else c(fields, selected)
      if (anyDuplicated(names(fields))) {
        return(unknown("Ambiguous output field."))
      }
    }
  }
  list(fields = fields, complete = TRUE, reason = NULL)
}
