normalize_target <- function(target) {
  if (is.null(target) || inherits(target, "dr_target")) {
    return(target)
  }
  method <- utils::getS3method(
    "dr_as_target",
    class(target)[[1]],
    optional = TRUE
  )
  if (!is.null(method)) {
    return(dr_as_target(target))
  }
  target
}
