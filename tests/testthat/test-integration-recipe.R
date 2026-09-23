test_that("recipes materialize the checked result before publication", {
  skip_if_not_installed("dataraft.adapters")
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "orders", data.frame(id = 1:3, amount = c(1, 2, 3)))
  flow <- dr_workflow() |>
    dr_add_product(dr_product("orders")) |>
    dr_add_source(dr_source_database(con, table = "orders")) |>
    dr_add_recipe(
      dr_recipe() |>
        dr_step_filter(id > 1) |>
        dr_step_mutate(amount = amount * 2)
    )
  result <- dr_trial(flow)
  expect_s3_class(result$data, "data.frame")
  expect_equal(dr_collect(result)$amount, c(4, 6))
  expect_identical(DBI::dbIsValid(con), TRUE)
})
