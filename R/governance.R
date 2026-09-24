#' Define a local organization policy
#'
#' Attach policies with [dr_add_policy()]. The `dataraft.policies` option can
#' supply additional organization-wide rules; both sources are evaluated.
#' A matching policy requires non-empty product or contract metadata fields.
#' Dotted paths such as `governance.retention` traverse nested lists.
#' @param id,version Stable policy identifier and version.
#' @param when Lifecycle event: validate, activate or publish.
#' @param require Character vector of required metadata paths.
#' @param classification Optional classification to which the policy applies.
#' @param action Block or warn when fields are absent.
#' @return A policy definition.
#' @export
 dr_policy <- function(id, when = "publish", require, version = "1", classification = NULL, action = c("block", "warn")) {
  scalar(id, "id")
  scalar(version, "version")
  if (!when %in% c("validate", "activate", "publish") || length(when) != 1L) {
    abort("when must be validate, activate or publish.", subclass = "dataraft_error_definition")
  }
  action <- match.arg(action)
  if (!is.character(require) || anyNA(require) || any(!nzchar(require)) || anyDuplicated(require)) {
    abort("require must contain unique non-empty metadata paths.", subclass = "dataraft_error_definition")
  }
  if (!is.null(classification)) scalar(classification, "classification")
  structure(list(id = id, version = version, when = when, require = require,
    classification = classification, action = action), class = "dr_policy")
}

#' Attach a governance policy to a product
#' @param product A DataRaft product definition.
#' @param policy A [dr_policy()] definition.
#' @return The updated product definition.
#' @export
dr_add_policy <- function(product, policy) {
  product <- editable_product(product)
  if (!inherits(policy, "dr_policy")) {
    abort("Supply a dr_policy definition.", subclass = "dataraft_error_definition")
  }
  policies <- product$policies %||% list()
  if (policy$id %in% vapply(policies, `[[`, character(1), "id")) {
    abort("Policy IDs must be unique on a product.", subclass = "dataraft_error_definition")
  }
  product$policies[[policy$id]] <- policy
  product
}

effective_policies <- function(product) {
  c(product$policies %||% list(), getOption("dataraft.policies", list()))
}

metadata_path <- function(product, path) {
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  if (parts[[1]] %in% c("owner", "description") && length(parts) == 1L) {
    value <- product[[parts[[1]]]] %||% product$contract[[parts[[1]]]]
  } else {
    value <- product$contract
    for (part in parts) {
      if (!is.list(value)) return(NULL)
      value <- value[[part]]
    }
  }
  value
}

#' Evaluate organization policies for a product
#' @param product DataRaft product.
#' @param policies List of [dr_policy()] definitions. Defaults to product
#'   policies plus additional organization-wide policies from the option.
#' @param event Lifecycle event.
#' @return A tibble with policy ID, version, decision, missing fields and time.
#' @export
 dr_check_policies <- function(product, policies = effective_policies(product), event = "publish") {
  if (!inherits(product, "dr_product") || !is.list(policies) ||
      any(!vapply(policies, inherits, logical(1), "dr_policy"))) {
    abort("Supply a product and a list of dr_policy definitions.", subclass = "dataraft_error_definition")
  }
  if (!event %in% c("validate", "activate", "publish") || length(event) != 1L) {
    abort("Invalid policy event.", subclass = "dataraft_error_definition")
  }
  empty <- tibble::tibble(id = character(), version = character(), event = character(),
    decision = character(), missing = character(), evaluated_at = as.POSIXct(character(), tz = "UTC"))
  rows <- lapply(policies, function(policy) {
    if (!identical(policy$when, event)) return(NULL)
    classification <- metadata_path(product, "governance.classification")
    if (!is.null(policy$classification) && !identical(classification, policy$classification)) return(NULL)
    missing <- policy$require[!vapply(policy$require, function(field) {
      value <- metadata_path(product, field)
      !is.null(value) && length(value) > 0L && !anyNA(value) &&
        (!is.character(value) || all(nzchar(value)))
    }, logical(1))]
    tibble::tibble(id = policy$id, version = policy$version, event = event,
      decision = if (!length(missing)) "pass" else policy$action,
      missing = paste(missing, collapse = ", "), evaluated_at = Sys.time())
  })
  dplyr::bind_rows(c(list(empty), rows))
}

 dr_assert_policies <- function(product, event) {
  decisions <- dr_check_policies(product, event = event)
  blocked <- decisions[decisions$decision == "block", , drop = FALSE]
  if (nrow(blocked)) {
    abort(paste0("Organization policy blocked ", event, ": ",
      paste(paste0(blocked$id, " (", blocked$missing, ")"), collapse = "; ")),
      subclass = "dataraft_error_policy")
  }
  invisible(decisions)
}
