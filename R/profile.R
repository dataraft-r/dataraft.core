#' Summarize columns without collecting their data
#'
#' Returns one row per selected column. Database and Arrow tables are reduced
#' with aggregate queries; only aggregate results and a zero-row prototype
#' reach R. Profiles contain no samples or frequent values. Exact counts can
#' still require a full backend scan, so select only the columns you need.
#'
#' Distinct counts exclude missing values. List-column distinct counts are
#' available for data frames and reported as `NA` for lazy tables. Minimum and
#' maximum are reported only for numeric, integer64, date and timestamp columns;
#' text output preserves large integer64 values without converting to doubles.
#' A profile describes observations and never creates quality rules implicitly.
#' @param x A data frame, lazy database table or Arrow table.
#' @param columns Character vector of column names; `NULL` selects all columns.
#' @returns A tibble with `column`, `type`, `n_rows`, `n_missing`, `n_distinct`,
#'   `min` and `max`. Unsupported or empty extrema are `NA_character_`.
#' @export
#' @examples
#' dr_profile_data(data.frame(id = c(1L, 1L, NA), amount = c(5, 10, 20)))
dr_profile_data <- function(x, columns = NULL) {
  prototype <- table_prototype(x)
  columns <- columns %||% names(prototype)
  if (
    !is.character(columns) ||
      anyNA(columns) ||
      anyDuplicated(columns) ||
      !all(columns %in% names(prototype))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "columns must contain unique column names present in the table."
    )
  }
  types <- infer_column_types(prototype[columns])
  if (!length(columns)) {
    return(tibble::tibble(
      column = character(),
      type = character(),
      n_rows = numeric(),
      n_missing = numeric(),
      n_distinct = numeric(),
      min = character(),
      max = character()
    ))
  }
  x <- dplyr::ungroup(x)
  lazy <- is_lazy_table(x)
  rows <- lapply(columns, function(column) {
    type <- types[[column]]
    ordered <- type %in% c("integer", "numeric", "integer64", "Date", "POSIXct")
    if (lazy) {
      variable <- rlang::sym(column)
      expressions <- list(
        n_rows = rlang::expr(dplyr::n()),
        n_missing = rlang::expr(sum(
          as.integer(is.na(!!variable)),
          na.rm = TRUE
        ))
      )
      if (type != "list") {
        expressions$n_distinct <- rlang::expr(dplyr::n_distinct(
          !!variable,
          na.rm = TRUE
        ))
      }
      if (ordered) {
        expressions$minimum <- rlang::expr(min(!!variable, na.rm = TRUE))
        expressions$maximum <- rlang::expr(max(!!variable, na.rm = TRUE))
      }
      summary <- dplyr::collect(dplyr::summarise(x, !!!expressions))
      n_rows <- as.numeric(summary$n_rows[[1]])
      n_missing <- if (n_rows == 0) 0 else as.numeric(summary$n_missing[[1]])
      n_distinct <- if (type == "list") {
        NA_real_
      } else {
        as.numeric(summary$n_distinct[[1]])
      }
      minimum <- if (ordered) summary$minimum else NULL
      maximum <- if (ordered) summary$maximum else NULL
    } else {
      value <- contract_column_value(x[[column]])
      n_rows <- nrow(x)
      n_missing <- sum(is.na(value))
      n_distinct <- dplyr::n_distinct(value, na.rm = TRUE)
      minimum <- maximum <- NULL
      if (ordered && n_rows > n_missing) {
        minimum <- min(value, na.rm = TRUE)
        maximum <- max(value, na.rm = TRUE)
      }
    }
    available <- ordered && n_rows > n_missing
    tibble::tibble(
      column = column,
      type = type,
      n_rows = as.numeric(n_rows),
      n_missing = as.numeric(n_missing),
      n_distinct = as.numeric(n_distinct),
      min = if (available) profile_extreme(minimum) else NA_character_,
      max = if (available) profile_extreme(maximum) else NA_character_
    )
  })
  dplyr::bind_rows(rows)
}


profile_extreme <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (inherits(x, "POSIXct")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
  }
  if (is.double(x) && !is.object(x)) {
    return(format(x, digits = 17, trim = TRUE, scientific = FALSE))
  }
  as.character(x)
}
