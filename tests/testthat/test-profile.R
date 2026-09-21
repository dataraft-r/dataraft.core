test_that("profiles have bounded aggregate columns and no sample values", {
  data <- data.frame(
    id = c(1L, 1L, NA),
    amount = c(5, 10, 20),
    label = c("secret", NA, "secret")
  )
  profile <- dr_profile_data(data)
  expect_equal(profile$column, names(data))
  expect_equal(profile$n_rows, rep(3, 3))
  expect_equal(profile$n_missing, c(1, 0, 1))
  expect_equal(profile$n_distinct, c(1, 3, 1))
  expect_equal(profile$min, c("1", "5", NA))
  expect_equal(profile$max, c("1", "20", NA))
  expect_false(any(grepl("secret", unlist(profile), fixed = TRUE)))
  expect_equal(dr_profile_data(data, "amount"), profile[2, ])
  expect_equal(dr_profile_data(dplyr::group_by(data, label)), profile)
  expect_equal(nrow(dr_profile_data(data, character())), 0L)
  expect_error(dr_profile_data(data, "absent"), "present in the table")
  empty <- dr_profile_data(data[0, ])
  expect_equal(empty$n_rows, c(0, 0, 0))
  expect_equal(empty$n_distinct, c(0, 0, 0))
  expect_true(all(is.na(empty$min)))
  expect_equal(
    dr_profile_data(data.frame(x = c(NA_real_, NA_real_)))$min,
    NA_character_
  )
})

test_that("profiles handle factors, nested columns and exact large integers", {
  data <- data.frame(label = factor(c("a", "b")))
  data$nested <- I(list(list(a = 1), list(a = 1)))
  profile <- dr_profile_data(data)
  expect_equal(profile$type, c("character", "list"))
  expect_equal(profile$n_distinct, c(2, 1))
  skip_if_not_installed("bit64")
  integers <- data.frame(
    id = bit64::as.integer64(c("9007199254740993", "9007199254740995"))
  )
  profile <- dr_profile_data(integers)
  expect_equal(profile$type, "integer64")
  expect_equal(profile$min, "9007199254740993")
  expect_equal(profile$max, "9007199254740995")
})

test_that("database profiles equal native summaries without downloading rows", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(
    id = c(1L, 1L, NA),
    amount = c(5, 10, 20),
    label = c("a", NA, "a")
  )
  data$date <- as.Date(c("2026-01-01", NA, "2026-01-03"))
  DBI::dbWriteTable(con, "profile_input", data)
  table <- dplyr::tbl(con, "profile_input")
  original_collect <- dplyr::collect
  collected_rows <- integer()
  local_family_bindings(
    collect = function(x, ...) {
      out <- original_collect(x, ...)
      collected_rows <<- c(collected_rows, nrow(out))
      out
    },
    .package = "dplyr"
  )
  expect_equal(dr_profile_data(table), dr_profile_data(data))
  expect_true(length(collected_rows) > 0L)
  expect_true(all(collected_rows <= 1L))
  expect_equal(
    dr_profile_data(dplyr::group_by(table, label)),
    dr_profile_data(data)
  )
  DBI::dbExecute(con, "DELETE FROM profile_input")
  expect_equal(dr_profile_data(table), dr_profile_data(data[0, ]))
})

test_that("Arrow profiles and quality checks use the same summaries", {
  skip_if_not_installed("arrow")
  data <- data.frame(id = c(1L, 1L, NA), amount = c(5, 10, 20))
  table <- arrow::Table$create(data)
  expect_equal(dr_profile_data(table), dr_profile_data(data))
  rule <- dr_quality_rule("positive", ~ amount > 5)
  expect_equal(dr_run_quality(rule, table), dr_run_quality(rule, data))
  definition <- dr_contract(
    columns = c(id = "integer", amount = "numeric"),
    required = character()
  )
  expect_equal(dr_validate(table, definition), dr_validate(data, definition))
  query <- dplyr::filter(table, amount > 5)
  expected <- dplyr::filter(data, amount > 5)
  expect_equal(dr_profile_data(query), dr_profile_data(expected))
  expect_equal(
    dr_validate(query, definition),
    dr_validate(expected, definition)
  )
})
