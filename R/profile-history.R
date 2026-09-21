#' Compare delivery profiles over time
#'
#' Capture aggregate observations with explicit input versions, then compare
#' missing-value fractions and schema with a baseline. This detects configured
#' changes, not statistically calibrated distribution drift. No row samples are
#' stored. Persist the returned table through any checked DataRaft target;
#' reading versioned RDS releases preserves a reproducible profile history.
#' @param x Data to profile.
#' @param version Explicit source version or release identifier.
#' @param at Observation time.
#' @param baseline,current Profiles produced by dr_profile_snapshot().
#' @param missing_threshold Maximum absolute change in missing-value fraction.
#' @returns A profile table, or a comparison with status and missing_change.
#' @export
dr_profile_snapshot <- function(x, version, at = Sys.time()) {
  scalar(version, "version")
  if (!inherits(at, "POSIXt") || length(at) != 1L || is.na(at)) {
    abort("at must be one timestamp.")
  }
  out <- dr_profile_data(x)
  out$version <- rep(version, nrow(out))
  out$observed_at <- rep(format(at, tz = "UTC", usetz = TRUE), nrow(out))
  out
}

#' @rdname dr_profile_snapshot
#' @export
dr_profile_compare <- function(baseline, current, missing_threshold = .02) {
  if (
    !is.numeric(missing_threshold) ||
      length(missing_threshold) != 1L ||
      !is.finite(missing_threshold) ||
      missing_threshold < 0 ||
      missing_threshold > 1
  ) {
    abort("missing_threshold must be between zero and one.")
  }
  for (profile in list(baseline, current)) {
    if (
      !is.data.frame(profile) ||
        !all(c("column", "type", "n_rows", "n_missing") %in% names(profile)) ||
        anyDuplicated(profile$column)
    ) {
      abort("Supply one profile per version, with unique columns.")
    }
    if (
      !is.numeric(profile$n_rows) ||
        !is.numeric(profile$n_missing) ||
        any(!is.finite(profile$n_rows)) ||
        any(!is.finite(profile$n_missing)) ||
        anyNA(profile[c("column", "type", "n_rows", "n_missing")]) ||
        any(
          profile$n_rows < 0 |
            profile$n_missing < 0 |
            profile$n_missing > profile$n_rows
        )
    ) {
      abort("Profile counts must be known and valid.")
    }
  }
  columns <- union(baseline$column, current$column)
  rows <- lapply(columns, function(column) {
    before <- baseline[baseline$column == column, , drop = FALSE]
    after <- current[current$column == column, , drop = FALSE]
    status <- if (!nrow(before)) {
      "added"
    } else if (!nrow(after)) {
      "removed"
    } else if (!identical(before$type, after$type)) {
      "type_changed"
    } else {
      "stable"
    }
    delta <- NA_real_
    if (nrow(before) && nrow(after) && before$n_rows > 0 && after$n_rows > 0) {
      delta <- after$n_missing / after$n_rows - before$n_missing / before$n_rows
      if (status == "stable" && abs(delta) > missing_threshold) {
        status <- "missingness_changed"
      }
    } else if (status == "stable") {
      status <- "insufficient_data"
    }
    tibble::tibble(column = column, status = status, missing_change = delta)
  })
  dplyr::bind_rows(rows)
}
