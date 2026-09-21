prepare_quality_candidate <- function(data, contract) {
  rules <- Filter(
    function(rule) identical(rule$action, "quarantine"),
    contract$rules
  )
  if (!length(rules)) {
    return(list(
      data = data,
      quality = dr_validate(data, contract, keep_errors = TRUE),
      quarantine = NULL
    ))
  }
  # Partition one materialized delivery, so predicates and the writer see the
  # same rows even if an upstream lazy source changes during execution.
  data <- dr_collect(data)
  rejected <- rep(FALSE, nrow(data))
  evidence <- list()
  for (rule in rules) {
    if (!identical(rule$engine, "r") || !inherits(rule$check, "formula")) {
      abort(
        "Quarantine requires a native row formula; aggregate and custom-engine checks cannot identify rows.",
        subclass = "dataraft_error_quality"
      )
    }
    units <- quality_formula_units(data, rule$check)$.dr_pass
    bad <- is.na(units) | !units
    rejected <- rejected | bad
    item <- from_counts(rule$name, sum(bad), length(bad), rule$severity, 0)
    item$engine <- "r"
    if (any(bad)) {
      item$status <- "warning"
    }
    item$message <- paste(sum(bad), "rows quarantined before publication.")
    item$stage <- "quarantine"
    evidence[[length(evidence) + 1L]] <- item
  }
  clean <- data[!rejected, , drop = FALSE]
  quality <- dr_validate(clean, contract, keep_errors = TRUE)
  errors <- attr(quality, "dr_errors")
  quality <- dplyr::bind_rows(quality, dplyr::bind_rows(evidence))
  attr(quality, "dr_errors") <- errors
  list(
    data = clean,
    quality = quality,
    quarantine = data[rejected, , drop = FALSE]
  )
}

#' Inspect rows removed before publication
#'
#' Quarantine rows are retained in memory, never sent to the normal target or
#' catalog. Persist them explicitly in an access-controlled destination if
#' required. A failed predicate evaluation blocks execution instead of routing
#' unknown rows. Empty clean deliveries still obey the contract's allow_empty.
#' @param result A trial or run result.
#' @returns A data frame, or NULL when no quarantine partition was created.
#' @export
dr_quarantine_rows <- function(result) {
  if (!inherits(result, "dr_run_result")) {
    abort("Supply a trial or run result.")
  }
  result$quarantine
}

#' Internal family implementation interface
#' @keywords internal
#' @usage NULL
#' @export
dr_internal_prepare_quality_candidate <- prepare_quality_candidate
