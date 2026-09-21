#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @export
#' @name adapter_remote_path

adapter_remote_path <- function(path) {
  rlang::local_error_call(rlang::caller_env())
  grepl("^[[:alpha:]][[:alnum:]+.-]*://", path)
}
