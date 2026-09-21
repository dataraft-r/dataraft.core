#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name simple_reader

simple_reader <- function(path) {
  rlang::local_error_call(rlang::caller_env())
  switch(
    tolower(tools::file_ext(path)),
    csv = function(path) utils::read.csv(path, check.names = FALSE),
    tsv = function(path) utils::read.delim(path, check.names = FALSE),
    rds = readRDS,
    xlsx = excel_reader,
    xls = excel_reader,
    abort(
      subclass = "dataraft_error_definition",
      "Supported file types are CSV, TSV, RDS and Excel. Supply reader for another format."
    )
  )
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name automatic_types

automatic_types <- function(columns) {
  rlang::local_error_call(rlang::caller_env())
  columns[columns == "integer"] <- "numeric"
  columns
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name automatic_schema

automatic_schema <- function(name, columns) {
  rlang::local_error_call(rlang::caller_env())
  contract <- dr_contract(
    paste0(name, ".schema"),
    version = paste0("auto-", fingerprint(columns)),
    columns = columns,
    required = character(),
    max_age_hours = NULL
  )
  contract$automatic_schema <- TRUE
  contract
}


excel_reader <- function(path) {
  rlang::local_error_call(rlang::caller_env())
  need("readxl")
  readxl::read_excel(path)
}
