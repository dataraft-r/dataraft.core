#' Revise or replace a contract without executing it
#'
#' For a standalone contract, supply named revision arguments in `...`, such as
#' `version`, `columns` or `rules`. The identity/version and guarantee-review
#' checks of [dr_contract_update()] apply. This is the preferred verb spelling;
#' `dr_contract_update()` is a deprecated compatibility name.
#'
#' For a product or workflow, `contract` replaces its existing attached contract.
#' To revise that specification instead, extract it, revise its version, then
#' attach it. Removal is idempotent and restores automatic structure inference;
#' additional product quality rules remain attached. Extraction fails when no
#' contract is attached. These functions never read sources or evaluate rules.
#' @param x A contract, product, or modular workflow for updating; a product or
#'   modular workflow for extraction and removal.
#' @param contract Replacement contract, named type vector or prototype list.
#'   Only used when `x` is a product or workflow.
#' @param ... Named revision arguments of [dr_contract_update()].
#' @returns An updated definition, or the extracted contract specification.
#' @export
#' @examples
#' spec <- dr_contract("orders", columns = c(id = "integer"))
#' revised <- dr_update_contract(spec, version = "2", columns = c(total = "numeric"))
#' product <- dr_product("orders") |> dr_add_contract(spec)
#' product <- dr_update_contract(product, revised)
#' dr_extract_contract(product)
#' dr_remove_contract(product)
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

#' @rdname dr_update_contract
#' @export
dr_extract_contract <- function(x) {
  if (inherits(x, "dr_product_workflow")) {
    x <- dr_extract_product(x)
  }
  x <- editable_product(x)
  if (is.null(x$contract)) {
    abort(
      "This product has no contract. Use dr_add_contract().",
      subclass = "dataraft_error_definition"
    )
  }
  x$contract
}

#' @rdname dr_update_contract
#' @export
dr_remove_contract <- function(x) {
  if (inherits(x, "dr_product_workflow")) {
    x$product <- dr_remove_contract(dr_extract_product(x))
    check_workflow_slots(x)
    return(x)
  }
  x <- editable_product(x)
  x$contract <- NULL
  x
}

#' Edit named primary sources without reading them
#'
#' Updating requires an existing source. Extraction returns its stored adapter,
#' function or data frame, never its executed result. If `name` is omitted,
#' exactly one source must exist. Removing an explicitly named absent source is
#' idempotent. Only sources owned by `x` are edited: extract the product first to
#' edit sources attached to a workflow's product. Lookup dependencies remain in
#' their recipe steps and are not primary source slots.
#' @param x A product or modular workflow definition.
#' @param source Replacement data frame, path, function or source adapter.
#' @param name Existing primary source name, or NULL for the only source.
#' @param reader Optional file reader, as in [dr_add_source()].
#' @returns An updated definition, or the extracted source specification.
#' @export
#' @examples
#' product <- dr_product("orders") |>
#'   dr_add_source(data.frame(id = 1L), name = "delivery")
#' dr_update_source(product, data.frame(id = 2L)) |> dr_extract_source()
#' dr_remove_source(product, "delivery")
dr_update_source <- function(x, source, name = NULL, reader = NULL) {
  name <- primary_source_name(x, name)
  dr_extract_source(x, name)
  dr_add_source(x, source, name = name, reader = reader, replace = TRUE)
}

#' @rdname dr_update_source
#' @export
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

#' @rdname dr_update_source
#' @export
dr_remove_source <- function(x, name = NULL) {
  name <- primary_source_name(x, name)
  if (!inherits(x, "dr_product_workflow")) {
    x <- editable_product(x)
  }
  x$sources[[name]] <- NULL
  if (inherits(x, "dr_product_workflow")) {
    check_workflow_slots(x)
  }
  x
}

primary_source_name <- function(x, name) {
  if (!inherits(x, "dr_product_workflow")) {
    editable_product(x)
  }
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
