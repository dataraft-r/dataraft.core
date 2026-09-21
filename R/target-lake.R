normalize_target <- function(target) {
  rlang::local_error_call(rlang::caller_env())
  if (is.character(target) || inherits(target, c("dr_lake", "dr_config"))) {
    return(dataraft.lake::dr_target_lake(target))
  }
  target
}
