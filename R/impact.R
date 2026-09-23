#' Assess a proposed contract change against known downstream objects
#'
#' The graph comes from [dr_lineage()] or a table with `from_id` and `to_id`.
#' Only recorded dependencies are covered. Unknown external consumers remain
#' outside the result and `coverage` is explicitly `registered_only`.
#' @param product Product whose contract is being revised.
#' @param proposed_contract Proposed contract definition.
#' @param lineage Registered lineage source or edge table.
#' @return A list containing the diff, affected IDs and coverage.
#' @export
 dr_impact <- function(product, proposed_contract, lineage) {
  if (!inherits(product, "dr_product") || !inherits(product$contract, "dr_contract")) {
    abort("product must have a contract.", subclass = "dataraft_error_contract")
  }
  diff <- dr_contract_diff(product$contract, proposed_contract)
  edges <- if (is.data.frame(lineage)) lineage else dr_lineage(lineage)
  if (!all(c("from_id", "to_id") %in% names(edges))) {
    abort("lineage must contain from_id and to_id.", subclass = "dataraft_error_definition")
  }
  frontier <- product$id
  seen <- product$id
  while (length(frontier)) {
    next_ids <- setdiff(unique(edges$to_id[edges$from_id %in% frontier]), seen)
    next_ids <- next_ids[!is.na(next_ids) & nzchar(next_ids)]
    seen <- c(seen, next_ids)
    frontier <- next_ids
  }
  structure(list(diff = diff, affected = setdiff(seen, product$id),
    breaking = any(is.na(diff$breaking) | diff$breaking),
    coverage = "registered_only"), class = "dr_impact")
}
