#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name run_result

run_result <- function(run, status, release = NA_character_, quality = NULL) {
  rlang::local_error_call(rlang::caller_env())
  structure(
    list(
      run_id = run,
      status = status,
      release_id = release,
      quality = quality
    ),
    class = "dr_run_result"
  )
}


#' Execute a product
#'
#' Checks the complete definition and dependency graph before acquiring data.
#' Sources are acquired once, transformations run in order, and contract and
#' quality failures block publication. Shared upstream products run once within
#' the same execution. Without a target, the result retains its checked table;
#' [dr_collect()] materializes lazy output when it is needed.
#'
#' Product execution options passed through `...` include:
#' * `data`: a new delivery replacing the sole primary input while retaining
#'   its name, transformations and checks. If the primary input is a product,
#'   its single-primary-input chain is followed to the ordinary delivery,
#'   retaining every intermediate product and gate. Ambiguous branches require
#'   an explicit name through `sources`.
#' * `sources`: a named list of replacements using [dr_set_sources()] names.
#'   Use either `data` or `sources`. The original definition and unselected
#'   pinned results remain unchanged. Both options replace whole inputs, not
#'   individual rows; partition replacement requires an explicit target policy.
#' * `execution`: an optional [dr_execution_config()] value, overriding defaults
#'   stored by `dr_product(execution = )`. Engine defaults
#'   propagate through dependencies; explicit step choices win. Destination and
#'   layer defaults apply only to the root product. Untargeted dependencies stay
#'   in memory, and existing dependency targets are preserved. Stored defaults
#'   on nested products are not activated during dependency execution.
#' * `stop_on_failure`: defaults to `TRUE`. Set `FALSE` to receive failed or
#'   blocked run results for programmatic inspection.
#' * `evidence`: an optional directory for durable run records. Defaults to
#'   `getOption("dataraft.evidence")`; no evidence directory is required.
#' * `cache`: lake targets default to `FALSE`. `TRUE` reuses a matching current
#'   release and requires an explicit product `code_version`. Live reference
#'   quality checks cannot reuse cached releases.
#' * `business_date` and `notify`: optional lake publication context and an
#'   existing notification callback.
#'
#' Selecting a quality engine through execution defaults preserves the declared
#' contract's fingerprint. Execution evidence separately records the resolved
#' checking engine. An explicit product code version still identifies its
#' execution choices; a changed business promise needs a new contract version.
#'
#' Metrics and dbt project specifications also have execution methods; see
#' [dataraft.metrics::dr_measure()] and [dataraft.dbt::dr_dbt_build()] for their operation-specific options/results.
#' Managed dbt projects also accept `sources`, a named list of successful lake
#' releases. Bindings are validated before the dbt command runs.
#' @param pipeline Product to execute.
#' @param lake Optional connected lake or lake configuration, overriding the
#'   product's target for this run. Prefer [dr_set_target()] in reusable definitions.
#' @param ... Execution options described above or provided by an adapter.
#'   For products, `write = FALSE` evaluates sources and quality through the same
#'   path while disabling target, catalog and evidence writes for all dependencies.
#'   It does not verify destination permissions, capacity or publication conflicts.
#' @returns For products, a run result containing status, timestamps, input and
#'   output descriptors, quality, metadata and lifecycle. On failure an error
#'   contains the same result in `condition$result`.
#' @seealso [dr_publish()], [dr_collect()], [dr_inspect()], [dr_run_history()]
#' @export
#' @examples
#' orders <- dr_product("orders") |>
#'   dr_add_source(data.frame(id = 1:2, amount = c(25, 75))) |>
#'   dr_add_quality(~ amount >= 0)
#' result <- dr_run(orders)
#' dr_collect(result)
#' dr_inspect(result)
dr_run <- function(pipeline, lake = NULL, ...) UseMethod("dr_run")


#' @export
dr_run.default <- function(pipeline, lake = NULL, ...) {
  dr_execute(pipeline, lake, ...)
}
