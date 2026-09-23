#' Extension backend protocols
#'
#' Provider packages implement these S3 protocols for their own specification
#' and result classes. The core does not import or call provider implementations.
#' Implementations must preserve the write gate, pinned release identity and
#' caller-owned connection lifetime. Unsupported objects fail explicitly.
#' @param x Backend specification, connected backend, source or result.
#' @param ... Operation-specific arguments forwarded to the provider method.
#' @returns The operation's backend result; see the provider documentation.
#' @name backend-protocols
#' @export
dr_connect_backend <- function(x, ...) UseMethod("dr_connect_backend")

#' @export
dr_connect_backend.default <- function(x, ...) {
  abort(
    "No dr_connect_backend() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_close_backend <- function(x, ...) UseMethod("dr_close_backend")

#' @export
dr_close_backend.default <- function(x, ...) {
  abort(
    "No dr_close_backend() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_resolve_release <- function(x, ...) UseMethod("dr_resolve_release")

#' @export
dr_resolve_release.default <- function(x, ...) {
  abort(
    "No dr_resolve_release() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_read_release_data <- function(x, ...) UseMethod("dr_read_release_data")

#' @export
dr_read_release_data.default <- function(x, ...) {
  abort(
    "No dr_read_release_data() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_release_table <- function(x, ...) UseMethod("dr_release_table")

#' @export
dr_release_table.default <- function(x, ...) {
  abort(
    "No dr_release_table() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_as_release_source <- function(x, ...) UseMethod("dr_as_release_source")

#' @export
dr_as_release_source.default <- function(x, ...) {
  abort(
    "No dr_as_release_source() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_land_source <- function(x, ...) UseMethod("dr_land_source")

#' @export
dr_land_source.default <- function(x, ...) {
  abort(
    "No dr_land_source() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_read_release_source <- function(x, ...) UseMethod("dr_read_release_source")

#' @export
dr_read_release_source.default <- function(x, ...) {
  abort(
    "No dr_read_release_source() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_publish_model_result <- function(x, ...) UseMethod("dr_publish_model_result")

#' @export
dr_publish_model_result.default <- function(x, ...) {
  abort(
    "No dr_publish_model_result() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_with_release_backend <- function(x, ...) UseMethod("dr_with_release_backend")

#' @export
dr_with_release_backend.default <- function(x, ...) {
  abort(
    "No dr_with_release_backend() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_read_model_release <- function(x, ...) UseMethod("dr_read_model_release")

#' @export
dr_read_model_release.default <- function(x, ...) {
  abort(
    "No dr_read_model_release() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_registry_data <- function(x, ...) UseMethod("dr_registry_data")

#' @export
dr_registry_data.default <- function(x, ...) {
  abort(
    "No dr_registry_data() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_check_pipeline <- function(x, ...) UseMethod("dr_check_pipeline")

#' @export
dr_check_pipeline.default <- function(x, ...) {
  abort(
    "No dr_check_pipeline() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_diagnostic_measurements <- function(x, ...) {
  UseMethod("dr_diagnostic_measurements")
}

#' @export
dr_diagnostic_measurements.default <- function(x, ...) {
  abort(
    "No dr_diagnostic_measurements() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_measurement_quality <- function(x, ...) UseMethod("dr_measurement_quality")

#' @export
dr_measurement_quality.default <- function(x, ...) {
  abort(
    "No dr_measurement_quality() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_backend_lineage <- function(x, ...) UseMethod("dr_backend_lineage")

#' @export
dr_backend_lineage.default <- function(x, ...) {
  abort(
    "No dr_backend_lineage() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_as_parquet_source <- function(x, ...) UseMethod("dr_as_parquet_source")

#' @export
dr_as_parquet_source.default <- function(x, ...) {
  abort(
    "No dr_as_parquet_source() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_as_target <- function(x, ...) UseMethod("dr_as_target")

#' @export
dr_as_target.default <- function(x, ...) {
  abort(
    "No dr_as_target() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_filter_metadata <- function(x, ...) UseMethod("dr_filter_metadata")

#' @export
dr_filter_metadata.default <- function(x, ...) {
  abort(
    "No dr_filter_metadata() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_replace_backend_sources <- function(x, ...) {
  UseMethod("dr_replace_backend_sources")
}

#' @export
dr_replace_backend_sources.default <- function(x, ...) {
  abort(
    "No dr_replace_backend_sources() method is registered for this object. Load its provider package or supply an adapter.",
    subclass = "dataraft_error_dependency"
  )
}

#' @rdname backend-protocols
#' @export
dr_configure_target <- function(x, ...) UseMethod("dr_configure_target")

#' @export
dr_configure_target.default <- function(x, ...) {
  abort(
    "This target does not support layer configuration.",
    subclass = "dataraft_error_definition"
  )
}
