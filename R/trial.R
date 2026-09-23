#' Try a product without invoking configured writers
#'
#' `r lifecycle::badge("deprecated")`
#'
#' Deprecated since 0.1.0.9005; removal is planned for 2027-01-01.
#' Compatibility shorthand for `dr_run(x, write = FALSE)`. Executes sources,
#' transformations and checks through the same execution path. The write flag
#' disables targets, catalogs and durable evidence throughout the dependency graph.
#' The original definition is unchanged. Source and transformation callbacks are
#' ordinary user code: their own side effects cannot be prevented by the framework.
#' Read-only access to existing published inputs is still allowed.
#' @param x Product or modular [dr_workflow()] definition.
#' @param data,sources Replacement delivery or named sources, as in [dr_run()].
#' @param stop_on_failure Raise errors by default, as in [dr_run()]. Set `FALSE`
#'   explicitly to inspect a failed result.
#' @returns An in-memory run result accepted by [dr_collect()] and [dataraft.metrics::dr_measure()].
#' @export
#' @examples
#' orders <- dr_product("orders", data.frame(amount = c(10, 20))) |>
#'   dr_add_quality(~ amount >= 0)
#' dr_trial(orders) |> dr_collect()
dr_trial <- function(x, data = NULL, sources = NULL, stop_on_failure = TRUE) {
  lifecycle::deprecate_soft(
    "0.1.0.9005",
    "dr_trial()",
    "dr_run()",
    details = "Use dr_run(x, write = FALSE)."
  )
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
