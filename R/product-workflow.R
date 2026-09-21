#' Assemble product specifications and reusable recipes
#'
#' An empty [dr_workflow()] combines one product specification, one optional
#' recipe, primary sources and an optional destination. Components remain
#' independent R values. `dr_add_*()` rejects occupied slots; `dr_update_*()` replaces
#' an existing slot and `dr_remove_*()` clears it. Extraction returns the definition,
#' never executed data. These edits never read sources or invoke writers.
#'
#' A product specification may carry its own sources, checks and destination.
#' Workflow sources and destinations must not conflict with those settings.
#' Keep preparation in the recipe when using this interface: products with
#' embedded transformations are rejected by `dr_add_product()`. Existing direct
#' product pipelines remain executable. Model products can be added directly;
#' prepare their member tables with recipes before assembling the dm model.
#'
#' [dr_trial()], [dr_run()] and [dr_publish()] compile the components into the existing
#' product execution graph, retaining its quality gates, dependency sharing,
#' lazy behavior and immutable release semantics. Use `data =` at execution to
#' bind a first primary input or replace the existing single primary delivery.
#' @param x A modular [dr_workflow()]. `dr_add_recipe()` also accepts a table product.
#' @param product A product specification carrying identity and output checks.
#' @param recipe A preparation specification from [dr_recipe()].
#' @returns An updated definition, or the extracted component.
#' @name workflow-components
#' @export
#' @examples
#' spec <- dr_product("orders") |>
#'   dr_add_contract(c(id = "integer", amount = "numeric")) |>
#'   dr_add_quality(~ amount >= 0)
#' preparation <- dr_recipe() |> dr_step_mutate(amount = round(amount, 2))
#' flow <- dr_workflow() |> dr_add_product(spec) |> dr_add_recipe(preparation)
#' dr_trial(flow, data = data.frame(id = 1:2, amount = c(10.123, 20))) |>
#'   dr_collect()
dr_add_product <- function(x, product) {
  assert_product_workflow(x)
  if (!is.null(x$product)) {
    abort(
      subclass = "dataraft_error_definition",
      "This workflow already has a product. Use dr_update_product()."
    )
  }
  check_workflow_product(product)
  x$product <- product
  check_workflow_slots(x)
  x
}


new_product_workflow <- function(code_version = NULL, execution = NULL) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.null(code_version)) {
    scalar(code_version, "code_version")
  }
  structure(
    list(
      product = NULL,
      recipe = NULL,
      sources = list(),
      target = NULL,
      code_version = code_version,
      execution = validate_stored_execution(execution)
    ),
    class = "dr_product_workflow"
  )
}


assert_product_workflow <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (!inherits(x, "dr_product_workflow")) {
    abort(
      subclass = "dataraft_error_definition",
      "Start with an empty dr_workflow() to compose product and recipe slots."
    )
  }
  invisible(x)
}


check_workflow_product <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (!inherits(x, "dr_product")) {
    abort(
      subclass = "dataraft_error_definition",
      "Supply a dr_product() specification."
    )
  }
  if (length(x$transforms)) {
    abort(
      subclass = "dataraft_error_definition",
      "Keep preparation in a recipe when using dr_add_product()."
    )
  }
  invisible(x)
}


check_workflow_slots <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (length(x$sources) && length(x$product$sources)) {
    abort(
      subclass = "dataraft_error_definition",
      "Sources are already set on the product. Supply sources in one place."
    )
  }
  if (!is.null(x$target) && !is.null(product_display_target(x$product))) {
    abort(
      subclass = "dataraft_error_definition",
      "A destination is already set on the product. Set it in one place."
    )
  }
  if (inherits(x$product, "dr_model_product") && length(x$recipe$steps)) {
    abort(
      subclass = "dataraft_error_definition",
      "Apply recipes to the model's member table products first."
    )
  }
  invisible(x)
}


#' @rdname workflow-components
#' @export
dr_add_recipe <- function(x, recipe) {
  assert_recipe(recipe)
  if (inherits(x, "dr_product_workflow")) {
    if (!is.null(x$recipe)) {
      abort(
        subclass = "dataraft_error_definition",
        "This workflow already has a recipe. Use dr_update_recipe()."
      )
    }
    x$recipe <- recipe
    check_workflow_slots(x)
    return(x)
  }
  append_recipe(editable_product(x), recipe)
}


append_recipe <- function(x, recipe) {
  rlang::local_error_call(rlang::caller_env())
  for (name in names(recipe$steps)) {
    x <- dr_add_transform(x, recipe$steps[[name]], name = name)
  }
  x
}


#' @rdname workflow-components
#' @export
dr_update_product <- function(x, product) {
  dr_extract_product(x)
  x$product <- NULL
  dr_add_product(x, product)
}


#' @rdname workflow-components
#' @export
dr_update_recipe <- function(x, recipe) {
  dr_extract_recipe(x)
  x$recipe <- NULL
  dr_add_recipe(x, recipe)
}


#' @rdname workflow-components
#' @export
dr_remove_product <- function(x) {
  assert_product_workflow(x)
  x$product <- NULL
  x
}


#' @rdname workflow-components
#' @export
dr_remove_recipe <- function(x) {
  assert_product_workflow(x)
  x$recipe <- NULL
  x
}


#' @rdname workflow-components
#' @export
dr_extract_product <- function(x) {
  assert_product_workflow(x)
  if (is.null(x$product)) {
    abort(
      subclass = "dataraft_error_definition",
      "This workflow has no product. Use dr_add_product()."
    )
  }
  x$product
}


#' @rdname workflow-components
#' @export
dr_extract_recipe <- function(x) {
  assert_product_workflow(x)
  if (is.null(x$recipe)) {
    abort(
      subclass = "dataraft_error_definition",
      "This workflow has no recipe. Use dr_add_recipe()."
    )
  }
  x$recipe
}


compile_product_workflow <- function(x, data = NULL, sources = NULL) {
  rlang::local_error_call(rlang::caller_env())
  check_workflow_slots(x)
  out <- dr_extract_product(x)
  check_workflow_product(out)
  if (length(x$sources)) {
    out$sources <- x$sources
  }
  if (!is.null(x$recipe)) {
    out <- append_recipe(out, x$recipe)
  }
  if (!is.null(x$target)) {
    out <- dr_set_target(out, x$target)
  }
  if (!is.null(x$execution)) {
    attr(out, "dr_execution_config") <- x$execution
  }
  if (!is.null(x$code_version)) {
    out$code_version <- x$code_version
  }
  if (!is.null(data) && !length(out$sources)) {
    out <- dr_add_source(out, data, name = out$id)
    data <- NULL
  }
  delivery_aliases(out)
  replace_execution_sources(out, data, sources)
}


#' @export
dr_run.dr_product_workflow <- function(
  pipeline,
  lake = NULL,
  ...,
  data = NULL,
  sources = NULL
) {
  dr_run(compile_product_workflow(pipeline, data, sources), lake = lake, ...)
}


#' @export
dr_publish.dr_product_workflow <- function(
  x,
  name = NULL,
  to = NULL,
  ...,
  data = NULL,
  sources = NULL
) {
  dr_publish(
    compile_product_workflow(x, data, sources),
    name = name,
    to = to,
    ...
  )
}


#' @export
dr_validate.dr_product_workflow <- function(data, contract = NULL, ...) {
  dr_validate(compile_product_workflow(data), contract = contract, ...)
  data
}


#' @export
dr_inspect.dr_product_workflow <- function(x, ...) {
  list(
    type = "product workflow",
    product = if (!is.null(x$product)) dr_inspect(x$product),
    recipe = if (!is.null(x$recipe)) dr_inspect(x$recipe),
    sources = lapply(x$sources, dr_inspect),
    target = dr_inspect(x$target),
    code_version = x$code_version,
    execution = workflow_execution_descriptor(x)
  )
}


#' @export
print.dr_product_workflow <- function(x, ...) {
  cat("<product workflow>\n")
  cat("Product:", x$product$id %||% "not set", "\n")
  cat("Recipe:", length(x$recipe$steps), "step(s)\n")
  cat("Sources:", length(x$sources) + length(x$product$sources), "\n")
  cat(
    "Destination:",
    dr_inspect(
      x$target %||% product_display_target(x$product) %||% x$execution$to
    )$type,
    "\n"
  )
  invisible(x)
}


workflow_execution_descriptor <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  config <- x$execution %||%
    attr(x$product, "dr_execution_config", exact = TRUE)
  if (is.null(config)) {
    return(NULL)
  }
  list(
    quality = config$quality,
    relationships = config$relationships,
    target = dr_inspect(config$to),
    layer = config$layer
  )
}
