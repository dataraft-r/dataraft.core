.last_failure <- new.env(parent = emptyenv())
.last_failure$result <- NULL

#' Retrieve the last failed execution in this R session
#'
#' Recover the result after a failed `dr_run(write = FALSE, stop_on_failure = FALSE, ...) |> dr_collect()` pipe.
#' The result retains checks and local exceptions for [dr_quality_report()],
#' [dr_quality_rows()] and [dr_quality_errors()]. Successful runs do not replace it.
#' Only the most recent execution error carrying a result is retained. Definition
#' errors do not replace it. The object can retain input data and connections;
#' call `dr_last_failure(clear = TRUE)` to release the package's reference.
#' Nothing is written to disk or sent to a catalog.
#' @param clear Return the retained result and then clear it from the session.
#' @returns The failed result, or `NULL` if none has been retained.
#' @export
#' @examples
#' product <- dr_product("orders", data.frame(amount = -1)) |>
#'   dr_add_quality(~ amount >= 0)
#' try(dr_run(write = FALSE, stop_on_failure = FALSE, product) |> dr_collect(), silent = TRUE)
#' dr_quality_report(dr_last_failure())
#' invisible(dr_last_failure(clear = TRUE))
dr_last_failure <- function(clear = FALSE) {
  flag(clear, "clear")
  result <- .last_failure$result
  if (clear) {
    .last_failure$result <- NULL
  }
  result
}
