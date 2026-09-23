#' Define a scheduled delivery guarantee
#' @param freshness Maximum age in hours, or NULL.
#' @param refresh Currently `daily` or `weekly`.
#' @param available_by Local time in HH:MM form.
#' @param timezone IANA timezone of the deadline.
#' @return An SLA definition.
#' @export
 dr_sla <- function(freshness = NULL, refresh = c("daily", "weekly"), available_by, timezone = "UTC") {
  refresh <- match.arg(refresh)
  if (!is.null(freshness) && (!is.numeric(freshness) || length(freshness) != 1L ||
      !is.finite(freshness) || freshness <= 0)) {
    abort("freshness must be a positive number of hours.", subclass = "dataraft_error_definition")
  }
  if (!is.character(available_by) || length(available_by) != 1L ||
      is.na(available_by) || !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", available_by)) {
    abort("available_by must be a local HH:MM time.", subclass = "dataraft_error_definition")
  }
  scalar(timezone, "timezone")
  if (!timezone %in% OlsonNames()) {
    abort("timezone must be an IANA timezone.", subclass = "dataraft_error_definition")
  }
  structure(list(freshness = freshness, refresh = refresh,
    available_by = available_by, timezone = timezone), class = "dr_sla")
}

#' Evaluate an SLA using the actual delivery timestamp
#'
#' Supply the expected business date explicitly. A missing delivery is pending
#' until the deadline and late thereafter. A delivery after the deadline remains
#' late; an earlier quality check does not count as a delivery.
#' @param sla SLA definition.
#' @param business_date Expected date (ISO date or Date).
#' @param delivered_at Actual successful publication time, or NULL.
#' @param at Time of evaluation.
#' @return A one-row tibble suitable for storing as evidence.
#' @export
 dr_check_sla <- function(sla, business_date, delivered_at = NULL, at = Sys.time()) {
  if (!inherits(sla, "dr_sla")) abort("Supply an SLA.", subclass = "dataraft_error_definition")
  date <- as.Date(business_date)
  if (length(date) != 1L || is.na(date) || as.character(date) != as.character(business_date)) {
    abort("business_date must be a valid ISO date.", subclass = "dataraft_error_definition")
  }
  valid_time <- function(x) inherits(x, "POSIXct") && length(x) == 1L && is.finite(as.numeric(x))
  if (!valid_time(at) || (!is.null(delivered_at) && !valid_time(delivered_at))) {
    abort("at and delivered_at must be valid POSIXct timestamps.", subclass = "dataraft_error_definition")
  }
  local_time <- paste(date, sla$available_by)
  deadline <- as.POSIXct(local_time, format = "%Y-%m-%d %H:%M", tz = sla$timezone)
  if (is.na(deadline) || format(deadline, "%Y-%m-%d %H:%M", tz = sla$timezone) != local_time) {
    abort("Deadline is invalid in this timezone (DST transition).", subclass = "dataraft_error_definition")
  }
  status <- if (is.null(delivered_at)) {
    if (at < deadline) "pending" else "missing"
  } else if (delivered_at > deadline) {
    "late"
  } else if (!is.null(sla$freshness) && difftime(at, delivered_at, units = "hours") > sla$freshness) {
    "stale"
  } else "met"
  tibble::tibble(business_date = date, deadline = deadline,
    delivered_at = if (is.null(delivered_at)) as.POSIXct(NA, tz = "UTC") else delivered_at,
    evaluated_at = at, status = status)
}
