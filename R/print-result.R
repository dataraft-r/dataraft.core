#' @export
print.dr_run_result <- function(x, ...) {
  cat(run_result_message(x), "\n")
  if (identical(x$validation_status, "unvalidated")) {
    cat(
      "Unvalidated: no declared contract. Only explicit checks were evaluated.\n"
    )
  }
  if (!x$status %in% c("completed", "published", "cached")) {
    cat(
      "Inspect dr_quality_report(result) for checks and dr_quality_rows(result) for affected rows.\n"
    )
  }
  invisible(x)
}
