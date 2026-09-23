#' Set or remove primary product sources
#'
#' Named inputs replace or add primary sources. A named `NULL` removes one;
#' `sources = NULL` clears all inputs. This edits definitions without I/O.
#' Read the stored definitions with `product$sources`.
#' @param x Product definition.
#' @param ... Named source values; use adapters for explicit readers.
#' @param sources Optional complete named source list, or NULL to clear it.
#' @param .recursive Replace named deliveries in a dependency graph instead of
#'   primary slots. This also supports managed dbt source bindings; NULL is not
#'   a valid recursive delivery. Existing targets and checks are preserved.
#' @returns An updated definition.
#' @export
#' @examples
#' dr_product("orders") |> dr_set_sources(delivery = data.frame(id = 1L)) |>
#'   dr_set_sources(delivery = NULL)
dr_set_sources <- function(x, ..., sources, .recursive = FALSE) {
  flag(.recursive, ".recursive")
  if (.recursive) {
    if (!missing(sources)) {
      abort("Use named deliveries for recursive replacement.")
    }
    return(replace_sources_list(x, list(...)))
  }
  values <- list(...)
  if (!missing(sources)) {
    if (length(values)) {
      abort("Use sources or named inputs, not both.")
    }
    if (!is.null(sources) && !is.list(sources)) {
      abort("sources must be a named list or NULL.")
    }
    values <- sources %||% list()
    x$sources <- list()
  }
  x <- editable_product(x)
  if (
    length(values) &&
      (is.null(names(values)) ||
        anyNA(names(values)) ||
        any(!nzchar(names(values))) ||
        anyDuplicated(names(values)))
  ) {
    abort("Supply uniquely named sources.", subclass = "dataraft_error_source")
  }
  for (name in names(values)) {
    if (is.null(values[[name]])) {
      x$sources[[name]] <- NULL
    } else {
      x <- dr_add_source(x, values[[name]], name = name, replace = TRUE)
    }
  }
  x
}
