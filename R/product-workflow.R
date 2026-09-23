#' Attach reusable preparation to a product
#'
#' Products own their sources, preparation, checks and destination. There is no
#' separate modular workflow object or execution method. Recipes remain reusable
#' definitions and never execute when attached.
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
