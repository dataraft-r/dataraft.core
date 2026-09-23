dr_update_contract <- function(x, contract = NULL, ...) {
  if (inherits(x, "dr_contract")) {
    if (!is.null(contract)) {
      abort(
        "Supply named revision arguments for a contract specification.",
        subclass = "dataraft_error_contract"
      )
    }
    return(dr_contract_update(x, ...))
  }
  rlang::check_dots_empty()
  dr_extract_contract(x)
  if (is.null(contract)) {
    abort(
      "Supply a replacement contract.",
      subclass = "dataraft_error_contract"
    )
  }
  dr_add_contract(x, contract)
}

dr_extract_contract <- function(x) {
  x <- editable_product(x)
  if (is.null(x$contract)) {
    abort(
      "This product has no contract. Use dr_add_contract().",
      subclass = "dataraft_error_definition"
    )
  }
  x$contract
}

dr_remove_contract <- function(x) {
  x <- editable_product(x)
  x$contract <- NULL
  x
}

dr_update_source <- function(x, source, name = NULL, reader = NULL) {
  name <- primary_source_name(x, name)
  dr_extract_source(x, name)
  dr_add_source(x, source, name = name, reader = reader, replace = TRUE)
}

dr_extract_source <- function(x, name = NULL) {
  name <- primary_source_name(x, name)
  if (!name %in% names(x$sources)) {
    abort(
      "This definition has no primary source with that name.",
      subclass = "dataraft_error_definition"
    )
  }
  x$sources[[name]]
}

dr_remove_source <- function(x, name = NULL) {
  name <- primary_source_name(x, name)
  x <- editable_product(x)
  x$sources[[name]] <- NULL
  x
}

primary_source_name <- function(x, name) {
  editable_product(x)
  if (is.null(name)) {
    if (length(x$sources) != 1L) {
      abort(
        "Name the primary source when the definition does not have exactly one.",
        subclass = "dataraft_error_definition"
      )
    }
    return(names(x$sources)[[1L]])
  }
  scalar(name, "name")
  name
}


#' Set or remove primary product sources
#'
#' Named inputs replace or add primary sources. A named `NULL` removes one;
#' `sources = NULL` clears all inputs. This edits definitions without I/O.
#' Read the stored definitions with `product$sources`.
#' @param x Product or modular workflow definition.
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
