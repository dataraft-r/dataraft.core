#' Review diagnostics in the IDE data viewer
#'
#' Opens a diagnostic table with [utils::View()] and returns that same table
#' invisibly. IDEs such as Positron and RStudio display it in their data viewer.
#' Use the diagnostic functions directly in headless sessions.
#'
#' `"rows"` delegates to [dr_quality_rows()] and collects at most `limit`
#' affected rows. Row predicates are re-evaluated against the retained candidate;
#' keep their captured constants unchanged. `"report"` uses [dr_quality_report()]
#' without exporting a file. `"lineage"` uses [dr_lineage()]. These last two
#' choices inspect metadata, not the source rows. No run is retried and the
#' retained last failure is not cleared.
#'
#' @param result A diagnostic input accepted by the selected function. Rows
#'   accept run/model results or a table with `rule` and, for named checks,
#'   `contract`. Reports also accept quality tables, run/dbt results and
#'   measurements. Lineage accepts the inputs documented by [dr_lineage()].
#'   Omit this argument to review [dr_last_failure()].
#' @param what Diagnostic table to display: affected rows, quality report or
#'   recorded dataset lineage.
#' @param rule,contract Arguments to [dr_quality_rows()]. Only valid for rows.
#'   Use `"table/check"` to choose a check from a model result.
#' @param limit Maximum affected rows to collect. Must be a finite non-negative
#'   whole number; defaults to 100. Only valid for rows. To explicitly request
#'   every affected row, call [dr_quality_rows()] directly instead.
#' @returns The displayed diagnostic tibble, invisibly.
#' @export
#' @examplesIf interactive()
#' orders <- dr_product("orders", data.frame(amount = c(10, -2))) |>
#'   dr_add_quality(list(positive = ~ amount >= 0))
#' result <- dr_trial(orders, stop_on_failure = FALSE)
#' dr_review(result, rule = "positive", limit = 20)
#' dr_review(result, what = "report")
dr_review <- function(
  result = dr_last_failure(),
  what = c("rows", "report", "lineage"),
  rule = NULL,
  contract = NULL,
  limit = 100
) {
  what <- match.arg(what)
  if (is.null(result)) {
    abort(
      "No failed result is retained. Supply a result to dr_review(), or run a workflow and retain its failed result first.",
      subclass = "dataraft_error_review"
    )
  }
  if (
    what != "rows" && (!missing(rule) || !missing(contract) || !missing(limit))
  ) {
    abort(
      "rule, contract and limit are only used with what = \"rows\".",
      subclass = "dataraft_error_review"
    )
  }
  if (
    what == "rows" &&
      (!is.numeric(limit) ||
        length(limit) != 1L ||
        is.na(limit) ||
        !is.finite(limit) ||
        limit < 0 ||
        limit != floor(limit))
  ) {
    abort(
      "limit must be a finite non-negative whole number for dr_review().",
      subclass = "dataraft_error_review"
    )
  }
  diagnostic <- switch(
    what,
    rows = dr_quality_rows(
      result,
      rule = rule,
      contract = contract,
      limit = limit
    ),
    report = dr_quality_report(result),
    lineage = dr_lineage(result)
  )
  title <- switch(
    what,
    rows = "DataRaft: affected rows",
    report = "DataRaft: quality report",
    lineage = "DataRaft: dataset lineage"
  )
  utils::View(diagnostic, title = title)
  invisible(diagnostic)
}
