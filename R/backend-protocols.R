#' Backend extension protocols
#'
#' These S3 interfaces keep storage ownership in adapters. Unsupported operations
#' fail explicitly. Methods must not perform I/O for source or target coercion.
#' Output readers return the pinned release, never silently select a newer one.
#' Diagnostic readers close owned connections after collecting bounded rows.
#' Write sessions retain coordination until the explicit caller scope exits.
#' Layer methods reject configurations unsupported by the adapter.
#' @param x A backend provider, destination or target adapter.
#' @param source A file source to archive.
#' @param asset,release Exact asset and release identifiers.
#' @param diagnostic Retained diagnostic descriptor.
#' @param rule Rule to diagnose.
#' @param limit Maximum diagnostic rows.
#' @param product,result Checked product definition and model result.
#' @param previous Optional previous publication for optimistic concurrency.
#' @param scope Execution environment owning the write-session lifetime.
#' @param layer Publication layer.
#' @param ... Adapter-specific options.
#' @returns Adapter-specific values as described by each operation.
#' @name backend-protocols
NULL

#' @rdname backend-protocols
#' @export
dr_as_target <- function(x, ...) UseMethod("dr_as_target")

#' @export
dr_as_target.default <- function(x, ...) {
  x
}

#' @rdname backend-protocols
#' @export
dr_as_source <- function(x, ...) UseMethod("dr_as_source")

#' @export
dr_as_source.default <- function(x, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_as_source() method.")
}

#' @rdname backend-protocols
#' @export
dr_land_source <- function(x, source, ...) UseMethod("dr_land_source")

#' @export
dr_land_source.default <- function(x, source, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_land_source() method.")
}

#' @rdname backend-protocols
#' @export
dr_output_source <- function(x, asset, release, ...) UseMethod("dr_output_source")

#' @export
dr_output_source.default <- function(x, asset, release, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_output_source() method.")
}

#' @rdname backend-protocols
#' @export
dr_read_output <- function(x, asset, release, ...) UseMethod("dr_read_output")

#' @export
dr_read_output.default <- function(x, asset, release, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_read_output() method.")
}

#' @rdname backend-protocols
#' @export
dr_read_diagnostic_rows <- function(x, diagnostic, rule = NULL, limit = 100, ...) UseMethod("dr_read_diagnostic_rows")

#' @export
dr_read_diagnostic_rows.default <- function(x, diagnostic, rule = NULL, limit = 100, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_read_diagnostic_rows() method.")
}

#' @rdname backend-protocols
#' @export
dr_publish_model_result <- function(x, product, result, previous = NULL, ...) UseMethod("dr_publish_model_result")

#' @export
dr_publish_model_result.default <- function(x, product, result, previous = NULL, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_publish_model_result() method.")
}

#' @rdname backend-protocols
#' @export
dr_acquire_write_session <- function(x, scope = parent.frame(), ...) UseMethod("dr_acquire_write_session")

#' @export
dr_acquire_write_session.default <- function(x, scope = parent.frame(), ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_acquire_write_session() method.")
}

#' @rdname backend-protocols
#' @export
dr_set_target_layer <- function(x, layer, ...) UseMethod("dr_set_target_layer")

#' @export
dr_set_target_layer.default <- function(x, layer, ...) {
  abort(subclass = "dataraft_error_definition",
    "This provider needs a dr_set_target_layer() method.")
}


#' Read an input with an optional execution resource
#' @param x Source adapter.
#' @param context Optional caller-owned read resource.
#' @param ... Reader-specific options.
#' @returns The same table as the source reader.
#' @export
dr_read_input <- function(x, context = NULL, ...) UseMethod("dr_read_input")

#' @export
dr_read_input.default <- function(x, context = NULL, ...) dr_read_source(x, ...)
