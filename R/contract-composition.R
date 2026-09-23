#' Add validation policy to a contract
#'
#' Keep identity, columns and keys in [dr_contract()] and compose policy only
#' when required. This builds a new definition without I/O. Changing a registered
#' contract still requires a new version before registration or publication.
#' @param x Contract specification.
#' @param required Non-null columns; keys always remain non-null.
#' @param constraints Named per-column min, max, enum, nullable, timezone or precision checks.
#' @param allow_empty,allow_extra Whether empty deliveries or extra columns are allowed.
#' @param max_age_hours Freshness limit, or NULL for no freshness monitoring.
#' @returns An updated contract specification.
#' @export
#' @examples
#' dr_contract("orders", columns = c(id = "integer"), key = "id") |>
#'   dr_contract_policy(allow_extra = TRUE)
dr_contract_policy <- function(
  x,
  required = x$required,
  allow_empty = x$allow_empty,
  allow_extra = x$allow_extra,
  max_age_hours = x$max_age_hours,
  constraints = x$constraints
) {
  if (!inherits(x, "dr_contract")) {
    abort("Supply a contract.", subclass = "dataraft_error_contract")
  }
  checked <- new_contract(
    x$id,
    columns = unlist(x$columns),
    key = x$key,
    rules = contract_user_rules(x),
    required = required,
    allow_empty = allow_empty,
    allow_extra = allow_extra,
    max_age_hours = max_age_hours,
    constraints = constraints
  )
  fields <- c(
    "required",
    "allow_empty",
    "allow_extra",
    "max_age_hours",
    "constraints",
    "rules"
  )
  x[fields] <- checked[fields]
  x
}

#' Add business metadata to a contract
#'
#' Metadata describes ownership and meaning; it does not enforce governance
#' policies. Existing contracts remain unchanged. Registration enforces versions.
#' @param x Contract specification.
#' @param ... Named metadata: owner, description, grain, producer, operator,
#'   governance, column_metadata or version.
#' @returns An updated contract specification.
#' @export
#' @examples
#' dr_contract("orders", columns = c(id = "integer")) |>
#'   dr_contract_meta(owner = "Analytics", grain = "One order")
dr_contract_meta <- function(x, ...) {
  if (!inherits(x, "dr_contract")) {
    abort("Supply a contract.", subclass = "dataraft_error_contract")
  }
  values <- list(...)
  allowed <- c(
    "owner",
    "description",
    "grain",
    "producer",
    "operator",
    "governance",
    "column_metadata",
    "version"
  )
  if (
    length(values) &&
      (is.null(names(values)) ||
        anyDuplicated(names(values)) ||
        any(!names(values) %in% allowed))
  ) {
    abort(
      "Supply unique named contract metadata fields.",
      subclass = "dataraft_error_contract"
    )
  }
  checked <- do.call(
    new_contract,
    c(list(id = x$id, columns = unlist(x$columns)), values)
  )
  x[names(values)] <- checked[names(values)]
  x
}

#' Define a product containing related tables
#'
#' Use a dm model for related tables and [dr_product()] for a single table.
#' @param id Product identifier.
#' @param model A dm model containing the member tables and relationships.
#' @param contracts Named member contracts.
#' @param ... Product identity and execution metadata passed to [dr_product()].
#' @returns A model product specification.
#' @export
dr_model_product <- function(id, model, contracts = NULL, ...) {
  if (!inherits(model, "dm")) {
    abort("Supply a dm model.", subclass = "dataraft_error_definition")
  }
  dr_product(id, data = model, contracts = contracts, ...)
}

contract_user_rules <- function(x) {
  generated <- unlist(
    lapply(names(x$constraints), function(column) {
      paste(
        "constraint",
        column,
        setdiff(names(x$constraints[[column]]), "nullable"),
        sep = ":"
      )
    }),
    use.names = FALSE
  )
  Filter(function(rule) !rule$name %in% generated, x$rules)
}
