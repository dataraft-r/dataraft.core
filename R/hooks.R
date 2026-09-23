#' Add a post-execution lifecycle hook
#'
#' Published and failed hooks receive sanitized run metadata after execution.
#' A deprecated hook runs after the persisted lake transition commits.
#' A hook failure is recorded as a warning and cannot roll back a committed
#' release. Callbacks may run again if the caller retries a publication; external
#' receivers should deduplicate with the event's run ID.
#' @param product DataRaft product.
#' @param event `published`, `failed` or `deprecated`.
#' @param callback Function receiving an event list.
#' @return Updated product.
#' @export
 dr_hook <- function(product, event, callback) {
  product <- editable_product(product)
  if (!is.character(event) || length(event) != 1L || is.na(event) ||
      !event %in% c("published", "failed", "deprecated") || !is.function(callback)) {
    abort("Supply a published, failed or deprecated event and a callback function.",
      subclass = "dataraft_error_definition")
  }
  product$hooks[[event]] <- c(product$hooks[[event]], list(callback))
  product
}

 run_product_hooks <- function(product, result) {
  event <- if (result$status == "published") "published" else
    if (result$status %in% c("blocked", "error", "missing")) "failed" else NULL
  if (is.null(event)) return(result)
  payload <- list(event = event, run_id = result$run_id, product = product$id,
    status = result$status, occurred_at = result$finished_at,
    metadata = result$metadata)
  for (callback in product$hooks[[event]]) {
    failure <- tryCatch({ callback(payload); NULL }, error = identity)
    if (inherits(failure, "error")) {
      result$warnings <- c(result$warnings,
        paste0("Lifecycle hook failed: ", conditionMessage(failure)))
    }
  }
  result
}
