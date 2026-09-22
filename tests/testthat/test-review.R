test_that("review defaults to the retained failure and invisibly returns displayed rows", {
  dr_last_failure(clear = TRUE)
  withr::defer(dr_last_failure(clear = TRUE))
  shown <- list()
  testthat::local_mocked_bindings(
    View = function(x, title) {
      shown <<- list(data = x, title = title)
    },
    .package = "utils"
  )
  expect_error(
    dr_review(),
    "No failed result is retained",
    class = "dataraft_error_review"
  )
  expect_length(shown, 0)
  product <- dr_product("orders", data.frame(amount = c(-2, 1, -3))) |>
    dr_add_contract(c(amount = "numeric")) |>
    dr_add_quality(list(positive = ~ amount >= 0))
  expect_error(
    dr_trial(product) |> dr_collect(),
    class = "dataraft_error_quality"
  )
  retained <- dr_last_failure()
  out <- withVisible(dr_review(limit = 1))
  expect_identical(out$visible, FALSE)
  expect_identical(out$value, shown$data)
  expect_equal(out$value$amount, -2)
  expect_identical(shown$title, "DataRaft: affected rows")
  expect_identical(dr_last_failure(), retained)
})

test_that("report and lineage show public metadata without collecting input rows", {
  orders <- dr_product("orders", data.frame(id = 1:3))
  totals <- dr_product("totals", orders) |>
    dr_add_recipe(dr_recipe() |> dr_step_summarise(n = dplyr::n()))
  result <- dr_run(totals)
  expected <- list(
    report = dr_quality_report(result),
    lineage = dr_lineage(result)
  )
  shown <- list()
  testthat::local_mocked_bindings(
    View = function(x, title) {
      shown <<- list(data = x, title = title)
    },
    .package = "utils"
  )
  testthat::local_mocked_bindings(dr_collect = function(...) {
    stop("must not collect")
  })
  for (what in names(expected)) {
    out <- withVisible(dr_review(result, what = what))
    expect_identical(out$visible, FALSE)
    expect_identical(out$value, expected[[what]])
    expect_identical(shown$data, expected[[what]])
  }
  expect_identical(shown$title, "DataRaft: dataset lineage")
  expect_equal(nrow(shown$data), 1L)
  report <- dr_review(dr_quality(result), what = "report")
  expect_identical(report, expected$report)
  expect_identical(shown$title, "DataRaft: quality report")
})

test_that("review requires a specific row check when several checks fail", {
  count <- 0L
  testthat::local_mocked_bindings(
    View = function(x, title) count <<- count + 1L,
    .package = "utils"
  )
  product <- dr_product("orders", data.frame(amount = c(-2, 101))) |>
    dr_add_quality(list(positive = ~ amount >= 0, small = ~ amount < 100))
  result <- dr_trial(product)
  expect_error(
    dr_review(result),
    "Several checks",
    class = "dataraft_error_quality"
  )
  expect_identical(count, 0L)
  expect_equal(dr_review(result, rule = "small")$amount, 101)
  expect_identical(count, 1L)
})

test_that("invalid choices and inapplicable row arguments never open a viewer", {
  testthat::local_mocked_bindings(
    View = function(...) stop("must not view"),
    .package = "utils"
  )
  result <- dr_trial(
    dr_product("orders", data.frame(amount = -1)) |>
      dr_add_quality(~ amount >= 0)
  )
  expect_error(dr_review(NULL), class = "dataraft_error_review")
  expect_error(dr_review(result, what = "unknown"), "arg.*should be one of")
  expect_error(
    dr_review(result, what = "report", limit = 1),
    class = "dataraft_error_review"
  )
  expect_error(
    dr_review(result, what = "lineage", rule = "positive"),
    class = "dataraft_error_review"
  )
  expect_error(
    dr_review(result, what = "report", contract = NULL),
    class = "dataraft_error_review"
  )
  for (limit in list(Inf, NA_real_, -1, 1.5, numeric(), c(1, 2), "1", NULL)) {
    expect_error(
      dr_review(result, limit = limit),
      class = "dataraft_error_review"
    )
  }
})

test_that("explicit tables support named and formula checks and empty previews", {
  testthat::local_mocked_bindings(
    View = function(x, title) invisible(NULL),
    .package = "utils"
  )
  data <- data.frame(amount = c(-1, 0, -2))
  contract <- dr_contract(
    columns = c(amount = "numeric"),
    rules = list(positive = ~ amount >= 0)
  )
  expect_equal(
    dr_review(data, rule = "positive", contract = contract)$amount,
    c(-1, -2)
  )
  expect_equal(dr_review(data, rule = ~ amount >= 0)$amount, c(-1, -2))
  expect_equal(nrow(dr_review(data, rule = ~ amount >= 0, limit = 0)), 0L)
})

test_that("lazy affected-row previews apply a database limit before collection", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  lazy <- dplyr::copy_to(con, data.frame(amount = -seq_len(1000)), "orders")
  original_collect <- dr_collect
  queries <- character()
  testthat::local_mocked_bindings(dr_collect = function(x, ...) {
    queries <<- c(queries, as.character(dbplyr::sql_render(x)))
    original_collect(x, ...)
  })
  testthat::local_mocked_bindings(
    View = function(x, title) invisible(NULL),
    .package = "utils"
  )
  rows <- dr_review(lazy, rule = ~ amount >= 0, limit = 5)
  expect_equal(nrow(rows), 5L)
  expect_length(queries, 1L)
  expect_match(queries[[1L]], "LIMIT 5", fixed = TRUE)
})

test_that("model results retain table/check selection", {
  skip_if_not_installed("dm")
  testthat::local_mocked_bindings(
    View = function(x, title) invisible(NULL),
    .package = "utils"
  )
  spec <- dr_product(
    "portfolio",
    dm::dm(orders = data.frame(amount = c(-1, 2))),
    contracts = list(
      orders = dr_contract(
        columns = c(amount = "numeric"),
        rules = list(positive = ~ amount >= 0)
      )
    )
  )
  result <- dr_trial(spec)
  expect_equal(dr_review(result, rule = "orders/positive")$amount, -1)
  expect_identical(
    dr_review(result, what = "report"),
    dr_quality_report(result)
  )
})
