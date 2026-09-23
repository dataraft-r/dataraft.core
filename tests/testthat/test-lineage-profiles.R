test_that("static lineage propagates sequential mutations without executing code", {
  recipe <- dr_recipe() |>
    dr_step_mutate(net = gross / 1.19, twice = net * 2) |>
    dr_step_select(id, twice)
  lineage <- dr_column_lineage(recipe, c("id", "gross"))
  expect_true(lineage$complete)
  expect_equal(lineage$fields$twice, "gross")
  expect_false(
    dr_column_lineage(
      dr_recipe() |> dr_step_mutate(z = stop("must not run")),
      "id"
    )$complete
  )
})
test_that("profiles distinguish drift, schema changes and insufficient observations", {
  before <- dr_profile_snapshot(data.frame(id = 1:3), "v1")
  after <- dr_profile_snapshot(data.frame(id = c(1L, NA, NA)), "v2")
  expect_equal(dr_profile_compare(before, after)$status, "missingness_changed")
  expect_equal(dr_profile_compare(before, before)$status, "stable")
  expect_error(dr_profile_compare(before, after, -1), "threshold")
})

test_that("catalog delivery retains safe lineage and governance across evidence sanitization", {
  received <- NULL
  contract <- dr_contract(
    "orders",
    columns = c(amount = "numeric", net = "numeric")
  ) |>
    dataraft.core::dr_contract_meta(
      governance = list(
        retention = "P7Y",
        tags = c("Finance.Reporting"),
        odcs = list(servers = list(password = "never-export"))
      ),
      column_metadata = list(amount = list(classification = "PII.Sensitive"))
    )
  product <- dr_product(
    "orders",
    data.frame(amount = 10),
    contract = contract
  ) |>
    dplyr::mutate(net = amount / 2) |>
    dr_add_catalog(function(metadata) received <<- metadata)
  result <- dr_run(product)
  expect_equal(result$status, "completed")
  expect_equal(received$column_lineage$fields$net, "amount")
  expect_equal(received$contract$governance$retention, "P7Y")
  expect_equal(
    received$contract$column_metadata$amount$classification,
    "PII.Sensitive"
  )
  expect_null(received$contract$governance$odcs)
  expect_false(grepl("never-export", jsonlite::toJSON(received), fixed = TRUE))
})
