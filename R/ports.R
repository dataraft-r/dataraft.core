#' Describe a data product input or output port
#'
#' A port records a consumer-facing contract and delivery settings. An output
#' binds to the existing target adapter. The first output is primary; further
#' outputs use adapters implementing [dr_write_target()]. Publications run in
#' order. They are not atomic across destinations; failures record which ports
#' have already committed.
#' @param id Stable port name.
#' @param source Input source passed to [dr_add_source()].
#' @param target Output target passed to [dr_set_target()].
#' @param contract Optional contract for the port.
#' @param version Port interface version.
#' @param access Descriptive access class, not an IAM grant.
#' @param sla Optional [dr_sla()] guarantee.
#' @return A port definition.
#' @export
 dr_input <- function(id, source, contract = NULL, version = "1", access = "internal", sla = NULL) {
  new_port("input", id, source, contract, version, access, sla)
}

#' @rdname dr_input
#' @export
 dr_output <- function(id, target, contract = NULL, version = "1", access = "internal", sla = NULL) {
  new_port("output", id, target, contract, version, access, sla)
}

 new_port <- function(direction, id, endpoint, contract, version, access, sla) {
  scalar(id, "id")
  scalar(version, "version")
  scalar(access, "access")
  if (!is.null(contract) && !inherits(contract, "dr_contract")) {
    abort("Port contract must be a DataRaft contract.", subclass = "dataraft_error_definition")
  }
  if (!is.null(sla) && !inherits(sla, "dr_sla")) {
    abort("Port SLA must be a DataRaft SLA.", subclass = "dataraft_error_definition")
  }
  structure(list(direction = direction, id = id, endpoint = endpoint,
    contract = contract, version = version, access = access, sla = sla), class = "dr_port")
}

#' Attach a declared input to a product
#' @param product DataRaft product.
#' @param port Input port.
#' @return Updated product.
#' @export
 dr_add_input <- function(product, port) {
  product <- editable_product(product)
  if (!inherits(port, "dr_port") || !identical(port$direction, "input")) {
    abort("Supply an input port.", subclass = "dataraft_error_definition")
  }
  if (port$id %in% names(product$input_ports)) {
    abort("Input port names must be unique.", subclass = "dataraft_error_definition")
  }
  product <- dr_add_source(product, port$endpoint, name = port$id)
  product$input_ports[[port$id]] <- port
  product
}

#' Attach a declared output to a product
#'
#' The first output is the primary target. Later outputs publish the same
#' checked delivery in order through `dr_write_target()`. Failure of a later
#' output leaves earlier commits in place and records per-port status in
#' `result$port_outputs`; use [dr_retry_ports()] with the original in-memory
#' result to resume only unpublished outputs.
#' Cache reuse is not supported with multiple outputs.
#' @param product DataRaft product.
#' @param port Output port.
#' @return Updated product.
#' @export
 dr_add_output <- function(product, port) {
  product <- editable_product(product)
  if (!inherits(port, "dr_port") || !identical(port$direction, "output")) {
    abort("Supply an output port.", subclass = "dataraft_error_definition")
  }
  if (port$id %in% names(product$output_ports)) {
    abort("Output port names must be unique.", subclass = "dataraft_error_definition")
  }
  if (length(product$output_ports) &&
      !component_method("dr_write_target", port$endpoint)) {
    abort("Additional outputs need an adapter with dr_write_target().",
      subclass = "dataraft_error_definition")
  }
  if (!length(product$output_ports) && !is.null(product$target)) {
    abort("The primary target is already configured. Define its output port first.",
      subclass = "dataraft_error_definition")
  }
  if (!is.null(port$contract) && !identical(port$contract, product$contract)) {
    abort("Output port contract must match the product contract.",
      subclass = "dataraft_error_contract")
  }
  if (!length(product$output_ports)) product <- dr_set_target(product, port$endpoint)
  product$output_ports[[port$id]] <- port
  product
}
