#' Try a product without invoking configured writers
#'
#' Compatibility shorthand for `dr_run(x, write = FALSE)`. Executes sources,
#' transformations and checks through the same execution path. The write flag
#' disables targets, catalogs and durable evidence throughout the dependency graph.
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
#'   dr_add_quality(~ amount >= 0)
#' dr_trial(orders) |> dr_collect()
dr_trial <- function(x, data = NULL, sources = NULL, stop_on_failure = FALSE) {
  if (!inherits(x, c("dr_product", "dr_product_workflow"))) {
    abort(
      "dr_trial() needs a product definition.",
      subclass = "dataraft_error_definition"
    )
  }
  dr_run(
    x,
    data = data,
    sources = sources,
    write = FALSE,
    stop_on_failure = stop_on_failure
  )
}
