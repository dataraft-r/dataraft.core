#' Attach reusable preparation to a product
#'
#' Products own their sources, preparation, checks and destination. There is no
#' separate modular workflow object or execution method. Recipes remain reusable
#' definitions and never execute when attached.
#' @section Lifecycle:
#' The old `dr_add_product()` builder is soft-deprecated until 2027-01-01.
#' Begin directly with [dr_product()] instead.
#' @param x A product definition.
#' @param recipe A recipe from [dr_recipe()].
#' @returns An updated product definition.
#' @name workflow-components
#' @export
#' @examples
#' preparation <- dr_recipe() |> dr_step_mutate(amount = round(amount, 2))
#' dr_product("orders", data.frame(amount = 10.123)) |>
#'   dr_add_recipe(preparation) |> dr_run(write = FALSE) |> dr_collect()
dr_add_recipe <- function(x, recipe) {
  assert_recipe(recipe)
  append_recipe(editable_product(x), recipe)
}

append_recipe <- function(x, recipe) {
  for (name in names(recipe$steps)) {
    x <- dr_add_transform(x, recipe$steps[[name]], name = name)
  }
  x
}

# The old empty-workflow spelling builds an ordinary product, never a wrapper.
new_product_workflow <- function(code_version = NULL, execution = NULL) {
  lifecycle::deprecate_soft(
    "0.1.0.9005",
    "dr_workflow()",
    "dr_product()",
    details = "Empty modular workflows are retired; compose directly on a product."
  )
  x <- dr_product(
    "workflow",
    code_version = code_version,
    execution = execution
  )
  attr(x, "dr_empty_product") <- TRUE
  x
}

#' @rdname workflow-components
#' @param product A replacement product definition for the deprecated builder.
#' @export
dr_add_product <- function(x, product) {
  lifecycle::deprecate_soft("0.1.0.9005", "dr_add_product()", "dr_product()")
  x <- editable_product(x)
  product <- editable_product(product)
  if (!isTRUE(attr(x, "dr_empty_product"))) {
    abort("This definition already has a product. Start from dr_product().")
  }
  if (length(x$sources) && length(product$sources)) {
    abort("Sources are already set on the product.")
  }
  if (!is.null(x$target) && !is.null(product$target)) {
    abort("A destination is already set on the product.")
  }
  if (length(x$sources)) {
    product$sources <- x$sources
  }
  if (!is.null(x$target)) {
    product$target <- x$target
  }
  if (!is.null(x$code_version)) {
    product$code_version <- x$code_version
  }
  config <- attr(x, "dr_execution_config", exact = TRUE)
  if (!is.null(config)) {
    attr(product, "dr_execution_config") <- config
  }
  product <- append_recipe(
    product,
    structure(list(steps = x$transforms), class = "dr_recipe")
  )
  product
}

#' @rdname workflow-components
#' @export
dr_update_recipe <- function(x, recipe) {
  x <- dr_remove_recipe(x)
  dr_add_recipe(x, recipe)
}

#' @rdname workflow-components
#' @export
dr_remove_recipe <- function(x) {
  x <- editable_product(x)
  x$transforms <- list()
  x
}

#' @rdname workflow-components
#' @export
dr_extract_recipe <- function(x) {
  x <- editable_product(x)
  structure(list(steps = x$transforms), class = "dr_recipe")
}

# Internal migration helpers for stored code; no public CRUD surface.
dr_extract_product <- function(x) editable_product(x)
dr_remove_product <- function(x) new_product_workflow()
dr_update_product <- function(x, product) {
  attr(x, "dr_empty_product") <- TRUE
  dr_add_product(x, product)
}

compile_product_workflow <- function(x, ...) {
  abort(
    "Legacy workflow objects must be rebuilt with dr_product() and dr_add_recipe().",
    subclass = "dataraft_error_definition"
  )
}
