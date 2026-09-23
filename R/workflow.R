#' Inspect the execution plan without running a product
#'
#' Shows ordered source, transformation, validation, publication and catalog
#' steps. No source is read and no callback is called. A materialization value
#' of `NA` means the component has not declared its lazy behavior; ordinary
#' functions may support either lazy or in-memory tables.
#' @param pipeline Product specification, including an incomplete definition.
#' @returns A tibble with position, step, id, target and materializes columns.
#'   The `complete` attribute reports whether structural validation succeeds.
#' @export
#' @examples
#' dr_product("orders") |>
#'   dr_add_source(data.frame(id = 1:2)) |>
#'   dr_add_recipe(dr_recipe() |> dr_step_transform(function(data) dplyr::filter(data, id > 1))) |>
#'   dr_plan()
dr_plan <- function(pipeline) {
  if (inherits(pipeline, "dr_product_workflow")) {
    return(product_plan(compile_product_workflow(pipeline)))
  }
  if (inherits(pipeline, "dr_product")) {
    return(product_plan(pipeline))
  }
  if (!inherits(pipeline, "dr_pipeline")) {
    abort(subclass = "dataraft_error_definition", "Use dr_pipeline() first.")
  }
  rows <- list()
  add <- function(step, id, target) {
    rlang::local_error_call(rlang::caller_env())
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      position = length(rows) + 1L,
      step = step,
      id = id,
      target = target
    )
  }
  for (name in names(pipeline$steps)) {
    x <- pipeline$steps[[name]]
    switch(
      name,
      land = add(name, x$id, "immutable original"),
      extract = add(name, "reader", x$layer),
      precheck = add(
        name,
        paste(x$id, x$version, sep = "@"),
        "before raw write"
      ),
      transform = for (item in x) {
        add(name, item$id, "candidate input")
      },
      validate = add(name, paste(x$id, x$version, sep = "@"), "candidate gate"),
      publish = add(name, x$asset, paste(x$layer, x$mode, sep = ":"))
    )
  }
  out <- if (length(rows)) {
    dplyr::bind_rows(rows)
  } else {
    tibble::tibble(
      position = integer(),
      step = character(),
      id = character(),
      target = character()
    )
  }
  attr(out, "complete") <- tryCatch(
    {
      dr_check_pipeline(pipeline)
      TRUE
    },
    error = function(e) FALSE
  )
  out
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @export
#' @name dr_execute

dr_execute <- function(object, lake = NULL, ...) UseMethod("dr_execute")

#' @export
#' @noRd
dr_execute.default <- function(object, lake = NULL, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "dr_run() supports products, metrics and dbt projects."
  )
}

#' @export
print.dr_contract <- function(x, ...) {
  cat("<contract>", x$id, "@", x$version, "\n")
  cat("Grain:", x$grain, "| Owner:", x$owner, "\n")
  cat(
    "Columns:",
    paste(names(x$columns), unlist(x$columns), sep = ":", collapse = ", "),
    "\n"
  )
  cat("Key:", paste(x$key, collapse = ", "), "| Rules:", length(x$rules), "\n")
  invisible(x)
}

#' @export
print.dr_source <- function(x, ...) {
  cat("<source_file>", x$id, "@", x$version, "| local file reader\n")
  invisible(x)
}
