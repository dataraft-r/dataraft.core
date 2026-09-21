#' Define an input source
#' @param id,version Source identity and version.
#' @param path Local input file.
#' @param reader Function receiving the immutable landed file path.
#' @param owner,description Metadata.
#' @return Source specification. Files are read only at execution time.
#' @export
#' @examples
#' source <- dr_source_file("orders.file", "orders.csv", reader = utils::read.csv)
#' source
dr_source_file <- function(
  id,
  path,
  reader = function(path) utils::read.csv(path),
  version = "1.0.0",
  owner = "",
  description = ""
) {
  asset_id(id)
  scalar(path, "path")
  scalar(version, "version")
  if (!is.function(reader)) {
    abort(subclass = "dataraft_error_source", "reader must be a function.")
  }
  structure(
    list(
      id = id,
      version = version,
      kind = "source",
      path = absolute_path(path),
      reader = reader,
      owner = owner,
      description = description
    ),
    class = "dr_source"
  )
}
