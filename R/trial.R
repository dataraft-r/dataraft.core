#' Try a product without invoking configured writers
#'
#' Executes the product's sources, transformations and checks, with targets,
#' catalogs and durable run evidence disabled throughout its dependency graph.
#' The original definition is unchanged. Source and transformation callbacks are
#' ordinary user code: their own side effects cannot be prevented by the framework.
#' Read-only access to existing published inputs is still allowed.
#' @param x Product or modular [dr_workflow()] definition.
#' @param data,sources Replacement delivery or named sources, as in [dr_run()].
#' @param stop_on_failure Defaults to `FALSE`: a failed trial returns a result
#'   for [dr_quality_report()] and [dr_quality_rows()]. Use `TRUE` to raise an error.
#'   [dr_run()] and [dr_publish()] still raise errors on failure by default.
#' @returns An in-memory run result accepted by [dr_collect()] and [dataraft.metrics::dr_measure()].
#' @export
#' @examples
#' orders <- dr_product("orders", data.frame(amount = c(10, 20))) |>
#'   dr_set_target("reporting-lake")
#' dr_trial(orders) |> dr_collect()
dr_trial <- function(x, data = NULL, sources = NULL, stop_on_failure = FALSE) {
  if (inherits(x, "dr_product_workflow")) {
    return(dr_trial(
      compile_product_workflow(x, data, sources),
      stop_on_failure = stop_on_failure
    ))
  }
  if (!inherits(x, "dr_product")) {
    abort(
      subclass = "dataraft_error_definition",
      "dr_trial() needs a product definition."
    )
  }
  if (inherits(x, "dr_model_product")) {
    x <- apply_execution_defaults(x, product_execution(x, NULL))
    x$target <- NULL
    attr(x, "dr_execution_config") <- NULL
    return(dr_run(
      x,
      data = data,
      sources = sources,
      stop_on_failure = stop_on_failure
    ))
  }
  x <- replace_execution_sources(x, data, sources)
  x <- apply_execution_defaults(x, product_execution(x, NULL))
  clear <- function(product) {
    rlang::local_error_call(rlang::caller_env())
    product$target <- NULL
    product$catalogs <- list()
    attr(product, "dr_execution_config") <- NULL
    sources <- lapply(product_sources(product), function(source) {
      if (inherits(source, "dr_product")) clear(source) else source
    })
    replace_product_sources(product, sources)
  }
  dr_run(clear(x), evidence = NULL, stop_on_failure = stop_on_failure)
}
