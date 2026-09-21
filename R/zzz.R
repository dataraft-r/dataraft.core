.onLoad <- function(libname, pkgname) {
  # Register against the owning namespace without re-exporting filter().
  # R 4.2's static S3 checker otherwise resolves stats::filter from the
  # attached package, although dispatch correctly uses dplyr's generic.
  registerS3method(
    "filter",
    "dr_product",
    filter.dr_product,
    envir = asNamespace("dplyr")
  )
}
