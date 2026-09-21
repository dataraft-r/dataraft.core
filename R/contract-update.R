#' Reuse and explicitly revise a contract
#'
#' Adds or replaces declared column types without reading data or executing
#' rules. New columns are optional unless explicitly included in `required`.
#' Change `id` for a derived product, or `version` for a revised definition of
#' the same contract. The original specification is unchanged.
#'
#' Contracts describe final output expectations. Deferred dplyr transformations
#' do not rewrite them. After renaming, remove the old column and add the new
#' one here. After aggregation, explicitly set the new `grain` and `key`.
#' A changed grain requires an explicit key, including `character()` when no
#' key is claimed. Removing or changing a key column also requires an explicit
#' key. Removing a required column requires an explicit `required` vector.
#'
#' Existing rules must be explicitly replaced or reaffirmed with `rules` after
#' a removal, type change or grain change. Rules may contain arbitrary R code,
#' so their dependencies cannot safely be inferred. This review prevents silent
#' inheritance of guarantees, but is not proof that revised rules are correct.
#' Metadata for removed columns is discarded; other metadata is inherited.
#' @param x A confirmed contract specification.
#' @param id,version Identifier and definition version. At least one must differ
#'   from `x`; version ordering is not inferred.
#' @param columns Named type vector or zero-length prototypes to add or replace,
#'   as in [dr_contract()]. Omit to retain the declared types.
#' @param remove Character vector of declared column names to remove.
#' @param ... Other named arguments of [dr_contract()] to replace in full, such as
#'   `required`, `key`, `grain`, `rules`, `owner` or `column_metadata`.
#' @returns A new `contract` specification. Use [dr_contract_diff()] to review it.
#' @seealso [dr_contract()], [dr_contract_diff()]
#' @export
#' @examples
#' orders <- dr_contract("orders", columns = c(id = "integer"), key = "id")
#' enriched <- orders |>
#'   dr_contract_update(id = "enriched_orders", columns = c(channel = "character"))
#' dr_contract_diff(orders, enriched)
dr_contract_update <- function(
  x,
  id = x$id,
  version = x$version,
  columns = NULL,
  remove = character(),
  ...
) {
  if (!inherits(x, "dr_contract")) {
    abort(
      subclass = "dataraft_error_contract",
      "x must be a contract specification."
    )
  }
  assert_contract_ready(x)
  if (identical(id, x$id) && identical(version, x$version)) {
    abort(
      subclass = "dataraft_error_contract",
      "Change id for a derived contract or version for a revised definition."
    )
  }
  changes <- list(...)
  if (
    length(changes) &&
      (is.null(names(changes)) ||
        any(!nzchar(names(changes))) ||
        anyDuplicated(names(changes)) ||
        !all(
          names(changes) %in%
            setdiff(
              names(formals(dr_contract)),
              c(
                "id",
                "version",
                "columns"
              )
            )
        ))
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "... must contain unique named contract arguments."
    )
  }
  if (
    !is.character(remove) || anyNA(remove) || !all(remove %in% names(x$columns))
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "remove must name declared contract columns."
    )
  }
  additions <- if (is.null(columns)) {
    list()
  } else {
    dr_contract(columns = columns)$columns
  }
  if (length(intersect(names(additions), remove))) {
    abort(
      subclass = "dataraft_error_contract",
      "A column cannot be both added and removed."
    )
  }
  replaced <- intersect(names(additions), names(x$columns))
  type_changes <- replaced[
    !vapply(
      replaced,
      \(name) {
        identical(additions[[name]], x$columns[[name]])
      },
      logical(1)
    )
  ]
  affected <- union(remove, type_changes)
  grain_changed <- "grain" %in%
    names(changes) &&
    !identical(changes$grain, x$grain)
  if (
    (grain_changed || length(intersect(affected, x$key))) &&
      !"key" %in% names(changes)
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "Supply key explicitly after changing grain or a key column; use character() for no key."
    )
  }
  if (
    length(intersect(remove, x$required)) &&
      !"required" %in% names(changes)
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "Supply required explicitly after removing a required column."
    )
  }
  if (
    (grain_changed || length(affected)) &&
      length(x$rules) &&
      !"rules" %in% names(changes)
  ) {
    abort(
      subclass = "dataraft_error_contract",
      "Review and supply rules explicitly after removing columns, changing types or changing grain."
    )
  }
  args <- unclass(x)
  attr(args, "dr_anonymous") <- NULL
  args$kind <- NULL
  args$id <- id
  args$version <- version
  types <- x$columns[setdiff(names(x$columns), remove)]
  types[names(additions)] <- additions
  args$columns <- unlist(types, use.names = TRUE)
  if (!is.null(args$column_metadata)) {
    args$column_metadata[remove] <- NULL
  }
  args[names(changes)] <- changes
  do.call(dr_contract, args)
}
