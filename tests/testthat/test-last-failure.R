test_that("failed pipes retain only the latest execution result", {
  dr_last_failure(clear = TRUE)
  withr::defer(dr_last_failure(clear = TRUE))
  expect_null(dr_last_failure())
  bad <- dr_product("bad", data.frame(amount = -1)) |>
    dr_add_quality(~ amount >= 0)
  expect_error(
    dr_run(write = FALSE, stop_on_failure = FALSE, bad) |> dr_collect(),
    class = "dataraft_error_quality"
  )
  first <- dr_last_failure()
  expect_identical(first$status, "blocked")
  expect_true(any(dr_quality_report(first)$status == "failed"))
  dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    dr_product("good", data.frame(amount = 1))
  ) |>
    dr_collect()
  expect_identical(dr_last_failure(), first)
  expect_error(dr_product(""))
  expect_identical(dr_last_failure(), first)
  broken <- dr_product("broken", data.frame(amount = 1)) |>
    dr_add_quality(~ amont > 0)
  expect_error(
    dr_run(write = FALSE, stop_on_failure = FALSE, broken) |> dr_collect()
  )
  expect_match(
    conditionMessage(dr_quality_errors(dr_last_failure())[[1]]),
    "amont"
  )
  last <- dr_last_failure(clear = TRUE)
  expect_identical(last$metadata$product, "broken")
  expect_null(dr_last_failure())
})
