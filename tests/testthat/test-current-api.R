test_that("the namespace contains no retired functions or migration wrappers", {
  retired <- c(
    "dr_trial",
    "dr_add_product",
    "dr_update_product",
    "dr_remove_product",
    "dr_extract_product",
    "dr_update_contract",
    "dr_remove_contract",
    "dr_extract_contract",
    "dr_update_source",
    "dr_remove_source",
    "dr_extract_source",
    "dr_replace_sources",
    "new_product_workflow",
    "compile_product_workflow"
  )
  expect_length(
    intersect(retired, ls(asNamespace("dataraft.core"), all.names = TRUE)),
    0L
  )
  expect_named(
    formals(dr_contract),
    c("id", "columns", "key", "rules", "version")
  )
  for (constructor in list(
    dr_quality_rule,
    dr_pointblank_checks,
    dr_quality_reference
  )) {
    expect_length(
      intersect(names(formals(constructor)), c("severity", "max_failure")),
      0L
    )
  }
  expect_error(dr_workflow(), class = "dataraft_error")
})
