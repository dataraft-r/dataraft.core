#' Add validation policy to a contract declaration
#'
#' Compose a contract without filling the complete constructor argument list.
#' These builders create a new declaration without I/O. When revising an already
#' registered contract, use [dr_update_contract()] to change its version explicitly.
#' @param x A contract declaration.
#' @param required,allow_empty,allow_extra,max_age_hours As in [dr_contract()].
#' @returns A new contract; the input is unchanged.
#' @export
#' @examples
#' dr_contract(columns = c(id = "integer"), key = "id") |>
#'   dr_contract_policy(allow_extra = TRUE) |>
#'   dr_contract_meta(owner = "Analytics")
dr_contract_policy <- function(
  x, required = x$required, allow_empty = x$allow_empty,
  allow_extra = x$allow_extra, max_age_hours = x$max_age_hours
) {
  rebuild_contract_declaration(x, list(
    required = required, allow_empty = allow_empty,
    allow_extra = allow_extra, max_age_hours = max_age_hours
  ))
}

#' Add descriptive metadata to a contract declaration
#'
#' @inherit dr_contract_policy description return
#' @param x A contract declaration.
#' @param owner,description,producer,operator,column_metadata,governance As in [dr_contract()].
#' @export
dr_contract_meta <- function(
  x, owner = x$owner, description = x$description, producer = x$producer,
  operator = x$operator, column_metadata = x$column_metadata %||% list(),
  governance = x$governance %||% list()
) {
  rebuild_contract_declaration(x, list(
    owner = owner, description = description, producer = producer,
    operator = operator, column_metadata = column_metadata, governance = governance
  ))
}

rebuild_contract_declaration <- function(x, changes) {
  if (!inherits(x, "dr_contract")) {
    abort("x must be a contract declaration.", subclass = "dataraft_error_contract")
  }
  assert_contract_ready(x)
  args <- unclass(x)[intersect(names(x), names(formals(dr_contract)))]
  args$columns <- unlist(x$columns, use.names = TRUE)
  generated <- contract_constraint_rules(
    x$constraints %||% list(), args$columns, x$required, x$key
  )$rules
  generated_names <- vapply(generated, `[[`, character(1), "name")
  args$rules <- Filter(function(rule) !rule$name %in% generated_names, x$rules)
  args[names(changes)] <- changes
  out <- do.call(dr_contract, args)
  if (isTRUE(attr(x, "dr_anonymous"))) attr(out, "dr_anonymous") <- TRUE
  out
}
