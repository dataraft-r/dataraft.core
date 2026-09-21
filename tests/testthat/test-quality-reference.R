test_that("reference rules count rows, composite keys and nulls consistently", {
  reference <- data.frame(id = c("a", "a", "b", NA), region = c(1L, 1L, 2L, 1L))
  data <- data.frame(
    customer = c("a", "a", "b", "b", NA),
    area = c(1L, 1L, 2L, 1L, 1L)
  )
  by <- c(customer = "id", area = "region")
  rule <- dr_quality_reference(reference, by)
  evidence <- dr_run_quality(rule, data)
  expect_equal(evidence$n_total, 5)
  expect_equal(evidence$n_failed, 2)
  expect_equal(evidence$status, "failed")
  expect_equal(
    dr_run_quality(
      dr_quality_reference(reference, by, na_matches = "na"),
      data
    )$n_failed,
    1
  )
  expect_true(rule$dynamic_reference)
  expect_false(grepl('"b"', jencode(rule), fixed = TRUE))
  expect_equal(dr_run_quality(rule, data[0, ])$status, "not_checked")
  expect_error(
    dr_run_quality(rule, data["customer"]),
    "key columns are missing"
  )
  expect_error(
    dr_quality_reference(reference, c(customer = "id", customer = "region")),
    "unique"
  )
})

test_that("reference callbacks are deferred and resolved for every check", {
  calls <- 0L
  reference <- function() {
    calls <<- calls + 1L
    data.frame(id = seq_len(calls))
  }
  rule <- dr_quality_reference(reference, "id")
  dr_check_component(rule)
  expect_equal(calls, 0L)
  expect_equal(dr_run_quality(rule, data.frame(id = 2L))$status, "failed")
  expect_equal(dr_run_quality(rule, data.frame(id = 2L))$status, "passed")
  expect_equal(calls, 2L)
})

test_that("same-database reference checks stay lazy and cross-backend copies are explicit", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(customer = c(1L, 1L, 3L, NA_integer_))
  reference <- data.frame(id = c(1L, 1L, 2L, NA_integer_))
  DBI::dbWriteTable(con, "orders", data)
  DBI::dbWriteTable(con, "customers", reference)
  orders <- dplyr::tbl(con, "orders")
  customers <- dplyr::tbl(con, "customers")
  by <- c(customer = "id")
  actual <- dr_run_quality(dr_quality_reference(customers, by), orders)
  expect_equal(
    actual,
    dr_run_quality(dr_quality_reference(reference, by), data)
  )
  expect_true(DBI::dbIsValid(con))
  expect_error(
    dr_run_quality(dr_quality_reference(reference, by), orders),
    "copy = TRUE"
  )
  expect_error(
    dr_run_quality(dr_quality_reference(customers, by), data),
    "copy = TRUE"
  )
  expect_equal(
    dr_run_quality(dr_quality_reference(reference, by, copy = TRUE), orders),
    actual
  )
  expect_equal(
    dr_run_quality(dr_quality_reference(customers, by, copy = TRUE), data),
    actual
  )
  testthat::skip_if_not_installed("dataraft.adapters")
  adapter <- dataraft.adapters::dr_source_database(
    con,
    table = "customers",
    lazy = TRUE
  )
  expect_equal(
    dr_run_quality(dr_quality_reference(adapter, by), orders),
    actual
  )
})
