test_that("a checked workflow needs no extension namespace", {
  flow <- dr_product("orders") |>
    dr_add_contract(c(id = "integer", amount = "numeric")) |>
    dr_add_quality(~ amount >= 0) |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = round(amount, 2)))
  bad <- dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    flow,
    data = data.frame(id = 1:3, amount = c(25, -75, 50))
  )
  expect_identical(bad$status, "blocked")
  expect_true(nrow(dr_quality_rows(bad)) > 0)
  good <- dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    flow,
    data = data.frame(id = 1:3, amount = c(25, 75, 50))
  )
  expect_equal(sum(dr_collect(good)$amount), 150)
})

test_that("conditions expose the family and subsystem", {
  err <- tryCatch(
    dr_contract("orders", columns = c(amount = "unknown")),
    error = identity
  )
  expect_s3_class(err, "dataraft_error")
  expect_s3_class(err, "dataraft_error_contract")
  expect_match(deparse(conditionCall(err)), "dr_contract")
})

test_that("a blocked run exposes rule counts to handlers", {
  product <- dr_product("orders", data.frame(amount = c(10, -1))) |>
    dr_add_quality(~ amount >= 0)
  err <- tryCatch(dr_run(product), error = identity)
  expect_s3_class(err, "dataraft_error_quality")
  expect_identical(err$result$status, "blocked")
  expect_true(any(err$n_failed > 0, na.rm = TRUE))
  expect_s3_class(err$checks, "data.frame")
})
