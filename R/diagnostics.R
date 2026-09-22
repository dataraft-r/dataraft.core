#' Inspect execution status across R and dbt workflows
#' @param x A connected lake, [dr_run()] result, [dataraft.dbt::dr_dbt_build()] result or a
#'   measurement or measurement set from [dataraft.metrics::dr_measure()], or a [dr_workflow()] result.
#' @param asset Optional asset ID when querying a lake.
#' @param ... Reserved for diagnostic provider options.
#' @returns A tibble with `engine`, `id`, `status`, `success`, `release_id`,
#'   `asset`, `outcome` and `message`. `outcome` normalizes native statuses to
#'   `succeeded`, `blocked`, `failed` or `skipped` (unknown states are `NA`).
#'   Missing deliveries are `blocked` because no acceptable input is available.
#'   A dbt process failure remains visible even when
#'   individual nodes passed. No raw stdout or stderr is included. Workflow
#'   results return a compact table with `step`, `status` and `success`.
#' @export
#' @examples
#' orders <- dr_product("orders", data.frame(id = 1:3))
#' dr_status(dr_run(orders))
dr_status <- function(x, asset = NULL, ...) {
  x <- diagnostic_dispatch_object(x)
  UseMethod("dr_status", x)
}

#' @export
dr_status.default <- function(x, asset = NULL, ...) {
  if (inherits(x, "dr_workflow_result")) {
    return(x$status)
  }
  if (inherits(x, "dr_run_result")) {
    return(tibble::tibble(
      engine = "dataraft",
      id = x$run_id,
      status = x$status,
      outcome = status_outcome(x$status),
      success = x$status %in% c("completed", "published", "cached"),
      release_id = x$release_id,
      asset = x$asset %||% NA_character_,
      message = run_result_message(x)
    ))
  }
  abort(
    subclass = "dataraft_error_definition",
    "x must be a lake, dataraft run result, measurement set or dbt result."
  )
}


#' Retrieve quality evidence for a run or release
#'
#' An asset selects its latest attempt, while `release` selects the evidence
#' for that exact published stand. Cached runs resolve to the original release's
#' checks. These choices prevent an older successful run hiding a recent
#' failure.
#' @param x A lake, run result, measurement, measurement set, dbt result or
#'   quality tibble.
#'   Measurement sets resolve their exact input releases using retained
#'   references. Unavailable references return explicit `not_checked` evidence.
#' @param run_id Run ID when `x` is a lake.
#' @param asset Asset ID. Required with `release`; otherwise selects the latest
#'   attempt. Supply at most one of `run_id` and `asset`.
#' @param release Exact release ID to inspect, together with `asset`.
#' @param ... Reserved for diagnostic provider options.
#' @returns A quality tibble. `failure_rate` is derived from available counts.
#'   Native pointblank thresholds are recorded in the JSON `details` column.
#' @seealso [dr_quality_report()], [dr_status()]
#' @export
#' @examples
#' contract <- dr_contract("orders", "1", "Analytics", "Orders", "One order",
#'   c(id = "integer"), key = "id")
#' dr_quality(dr_validate(data.frame(id = c(1L, 1L)), contract))
dr_quality <- function(x, run_id = NULL, asset = NULL, release = NULL, ...) {
  x <- diagnostic_dispatch_object(x)
  UseMethod("dr_quality", x)
}

#' @export
dr_quality.default <- function(x, run_id = NULL, asset = NULL, release = NULL, ...) {
  if (inherits(x, "dr_run_result")) {
    x <- run_result_evidence(x)
    if (is.null(x$quality)) {
      return(dr_quality(quality_row(
        "execution",
        "not_checked",
        message = "No retained quality evidence is available for this result."
      )))
    }
    out <- x$quality
  } else if (
    is.data.frame(x) &&
      all(c("rule", "status", "n_failed", "n_total") %in% names(x))
  ) {
    out <- x
  } else {
    abort(
      subclass = "dataraft_error_definition",
      "x must be quality results, a lake, a run result, a measurement or a dbt result."
    )
  }
  out$failure_rate <- ifelse(
    is.finite(out$n_total) & out$n_total > 0,
    out$n_failed / out$n_total,
    NA_real_
  )
  out
}


#' Follow declared dataset lineage
#'
#' Traverses dataset IDs while retaining version IDs on each returned edge.
#' It does not infer column lineage or reconstruct a single historical DAG.
#' Run results expose only recorded inputs for that exact execution, without
#' querying the latest lake state or inferring unrecorded upstream edges.
#' @param x Connected lake, run result, measurement, measurement set, dbt result or dbt
#'   artifact directory. Measurement edges describe the input release and
#'   metric version; they have no execution run ID.
#' @param asset Optional starting dataset ID (dbt unique ID for dbt inputs).
#' @param direction Follow upstream inputs or downstream consumers.
#' @param recursive Follow all reachable edges, with cycle protection.
#' @returns A tibble of lineage edges with endpoint IDs and versions. dbt edges
#'   have empty version fields because a manifest declares dependencies.
#' @export
#' @examples
#' orders <- dr_product("orders", data.frame(id = 1:3))
#' totals <- dr_product("totals", orders) |>
#'   dr_add_recipe(dr_recipe() |> dr_step_summarise(n = dplyr::n()))
#' dr_lineage(dr_run(totals))
dr_lineage <- function(
  x,
  asset = NULL,
  direction = c("upstream", "downstream"),
  recursive = TRUE
) {
  direction <- match.arg(direction)
  flag(recursive, "recursive")
  edges <- dr_lineage_edges(diagnostic_dispatch_object(x))
  if (is.null(asset)) {
    return(edges)
  }
  scalar(asset, "asset")
  start <- if (direction == "upstream") "to_id" else "from_id"
  end <- if (direction == "upstream") "from_id" else "to_id"
  frontier <- asset
  seen <- character()
  selected <- rep(FALSE, nrow(edges))
  while (length(frontier)) {
    found <- edges[[start]] %in% frontier
    selected <- selected | found
    if (!recursive) {
      break
    }
    seen <- union(seen, frontier)
    frontier <- setdiff(unique(edges[[end]][found]), seen)
  }
  edges[selected, ]
}


status_outcome <- function(status) {
  rlang::local_error_call(rlang::caller_env())
  out <- rep(NA_character_, length(status))
  out[
    status %in% c("completed", "published", "cached", "success", "pass", "warn")
  ] <- "succeeded"
  out[status == "unvalidated"] <- "unvalidated"
  out[status %in% c("blocked", "fail", "missing")] <- "blocked"
  out[status %in% c("error", "failed", "runtime error")] <- "failed"
  out[status %in% c("skipped", "skip")] <- "skipped"
  out
}


empty_lineage <- function() {
  rlang::local_error_call(rlang::caller_env())
  tibble::tibble(
    run_id = character(),
    from_id = character(),
    from_version = character(),
    to_id = character(),
    to_version = character(),
    relation = character()
  )
}


run_result_lineage <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  recorded <- x$metadata$lineage
  if (is.data.frame(recorded)) {
    return(tibble::as_tibble(recorded)[names(empty_lineage())])
  }
  inputs <- x$source_inputs %||% x$inputs
  destination <- x$asset %||% x$metadata$product
  if (
    !is.data.frame(inputs) ||
      !all(c("asset", "run_id", "release_id") %in% names(inputs)) ||
      is.null(destination)
  ) {
    return(empty_lineage())
  }
  known <- !is.na(inputs$asset) & nzchar(inputs$asset)
  inputs <- inputs[known, , drop = FALSE]
  if (!nrow(inputs)) {
    return(empty_lineage())
  }
  version <- function(release, run) {
    rlang::local_error_call(rlang::caller_env())
    ifelse(!is.na(release) & nzchar(release), release, run)
  }
  unique(tibble::tibble(
    run_id = rep(x$run_id, nrow(inputs)),
    from_id = inputs$asset,
    from_version = version(inputs$release_id, inputs$run_id),
    to_id = rep(destination, nrow(inputs)),
    to_version = rep(version(x$release_id, x$run_id), nrow(inputs)),
    relation = rep("consumed_from", nrow(inputs))
  ))
}


# Keep execution errors out of summaries: they may contain credentials or rows.
#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name run_result_message

run_result_message <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  name <- x$asset %||% x$metadata$product %||% "This run"
  state <- switch(
    x$status,
    completed = "completed; the result is ready to collect.",
    unvalidated = "has no declared contract; its exploratory data are available without a validation guarantee.",
    published = "was published; the saved result is ready to collect.",
    cached = "reused its published result; it is ready to collect.",
    blocked = "is blocked; no successful output is available.",
    missing = "is blocked because an input delivery is missing.",
    error = "failed during execution; no successful output is available.",
    failed = "failed during execution; no successful output is available.",
    skipped = "was skipped; no successful output is available.",
    "has no confirmed successful output."
  )
  message <- paste(name, state)
  reason <- run_result_known_reason(x$error)
  if (!is.null(reason)) {
    message <- paste(message, reason)
  }
  checks <- x$quality
  if (!is.data.frame(checks) || !nrow(checks)) {
    upstream <- run_result_evidence(x)
    if (
      !identical(upstream, x) &&
        is.data.frame(upstream$quality) &&
        nrow(upstream$quality)
    ) {
      return(paste(message, "Upstream:", run_result_message(upstream)))
    }
    return(message)
  }
  blocked <- checks[!checks$status %in% c("passed", "warning"), , drop = FALSE]
  if (nrow(blocked)) {
    # Counts belong to each rule, not distinct rows across different checks.
    detail <- vapply(
      seq_len(min(3L, nrow(blocked))),
      function(i) {
        check <- blocked[i, ]
        count <- if (is.finite(check$n_failed) && is.finite(check$n_total)) {
          paste0(" (", check$n_failed, " of ", check$n_total, " checks failed)")
        } else {
          ""
        }
        paste0(check$rule, ": ", check$status, count)
      },
      character(1)
    )
    extra <- if (nrow(blocked) > 3L) {
      paste0("; ", nrow(blocked) - 3L, " more")
    } else {
      ""
    }
    message <- paste(
      message,
      paste0(
        nrow(blocked),
        " check",
        if (nrow(blocked) == 1L) "" else "s",
        " requiring attention",
        ": ",
        paste(detail, collapse = "; "),
        extra,
        "."
      )
    )
  } else if (any(checks$status == "warning")) {
    message <- paste(
      message,
      "Quality warnings are available in dr_quality(result)."
    )
  }
  message
}


# Preserve the original condition on result$error, but never print its payload.
#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name run_result_parent

run_result_parent <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (is.null(x$error)) {
    return(NULL)
  }
  rlang::error_cnd(
    "dr_execution_cause",
    message = "An execution error was retained in condition$result$error for local inspection."
  )
}


is_measurement <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  is.data.frame(x) && is.list(attr(x, "dr_manifest"))
}


run_result_evidence <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  # Traverse only structured result references, never exception text.
  # A fixed bound also handles malformed cyclic adapter conditions.
  for (i in seq_len(20L)) {
    if (is.data.frame(x$quality) && nrow(x$quality)) {
      return(x)
    }
    upstream <- x$error$result
    if (!inherits(upstream, "dr_run_result")) {
      return(x)
    }
    x <- upstream
  }
  x
}


run_result_known_reason <- function(error) {
  rlang::local_error_call(rlang::caller_env())
  # Only typed package conditions expose allowlisted metadata and fixed advice.
  for (i in seq_len(20L)) {
    if (!inherits(error, "condition")) {
      return(NULL)
    }
    if (inherits(error, "dr_definition_changed")) {
      return(paste0(
        "Definition changed without a version bump: ",
        error$definition_id,
        " ",
        error$definition_version,
        ". Review the definition and give the changed definition a new version before publishing."
      ))
    }
    if (inherits(error, "dr_read_only")) {
      return(
        "The target is read-only. Choose a writable target before publishing."
      )
    }
    error <- error$parent
  }
  NULL
}


# Older serialized measurements carried only the manifest attribute.
# Promote that documented representation before public S3 dispatch.
diagnostic_dispatch_object <- function(x) {
  if (is_measurement(x) && !inherits(x, "dr_measurement")) {
    class(x) <- c("dr_measurement", class(x))
  }
  x
}

#' Supply dataset lineage to the common traversal
#'
#' Extension methods return recorded edges only. The core performs traversal
#' and cycle protection through [dr_lineage()]. No data rows are returned.
#' @param x A result or metadata provider.
#' @param ... Reserved for provider options.
#' @returns A data frame with `run_id`, `from_id`, `from_version`, `to_id`,
#'   `to_version` and `relation` character columns.
#' @export
dr_lineage_edges <- function(x, ...) UseMethod("dr_lineage_edges")

#' @export
dr_lineage_edges.dr_run_result <- function(x, ...) run_result_lineage(x)

#' @export
dr_lineage_edges.default <- function(x, ...) {
  abort(subclass = "dataraft_error_definition",
    "This object needs a dr_lineage_edges() method.")
}

#' Read filtered registry metadata
#'
#' Providers validate table names and bind filters, never interpolate them.
#' @param lake A metadata provider.
#' @param table Registry table identifier.
#' @param asset,run_id Optional exact filters.
#' @param ... Reserved for provider options.
#' @returns A data frame of matching metadata, without product rows.
#' @export
dr_metadata_rows <- function(lake, table, asset = NULL, run_id = NULL, ...) {
  UseMethod("dr_metadata_rows")
}

#' @export
dr_metadata_rows.default <- function(lake, table, asset = NULL, run_id = NULL, ...) {
  abort(subclass = "dataraft_error_definition",
    "This object needs a dr_metadata_rows() method.")
}

metadata_filter <- dr_metadata_rows
