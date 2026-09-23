test_that("ordinary R prototypes preserve values and freshness is opt-in", {
  data <- data.frame(id = 1:2, label = factor(c("b", "a")))
  data$nested <- I(list(list(a = 1), list(a = 2)))
  definition <- dr_contract(
    columns = list(
      id = integer(),
      label = factor(),
      nested = list()
    )
  )
  expect_null(definition$max_age_hours)
  expect_equal(
    definition$columns,
    list(
      id = "integer",
      label = "character",
      nested = "list"
    )
  )
  dr_expect_quality(dr_validate(data, definition))
  expect_s3_class(data$label, "factor")
  expect_equal(
    infer_column_types(data),
    c(
      id = "integer",
      label = "character",
      nested = "list"
    )
  )
  plain_labels <- data
  plain_labels$label <- as.character(data$label)
  dr_expect_quality(dr_validate(plain_labels, definition))
  wrong <- dr_contract(columns = c(label = "integer"))
  expect_equal(dr_validate(data["label"], wrong)$status[[2]], "failed")
})

test_that("integer64 contracts distinguish exact values from floating point", {
  skip_if_not_installed("bit64")
  data <- data.frame(
    id = bit64::as.integer64(c("9007199254740993", "9007199254740995"))
  )
  definition <- dr_contract(columns = list(id = bit64::integer64()))
  expect_equal(definition$columns$id, "integer64")
  dr_expect_quality(dr_validate(data, definition))
  expect_equal(as.character(data$id), c("9007199254740993", "9007199254740995"))
  expect_equal(
    dr_validate(data, dr_contract(columns = c(id = "numeric")))$status[[2]],
    "failed"
  )
  expect_equal(
    dr_validate(data.frame(id = c(1, 2)), definition)$status[[2]],
    "failed"
  )
})

test_that("database integer64 profiles and validation preserve exact values", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(
    id = bit64::as.integer64(c(
      "9007199254740993",
      "9007199254740995"
    ))
  )
  DBI::dbWriteTable(con, "large_identifiers", data)
  table <- dplyr::tbl(con, "large_identifiers")
  expect_equal(dr_profile_data(table), dr_profile_data(data))
  dr_expect_quality(dr_validate(
    table,
    dr_contract(columns = c(id = "integer64"))
  ))
})

test_that("lake publication roundtrips integer64 values above double precision", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")
  lake <- dr_open_lake(withr::local_tempdir(), backend = "duckdb")
  withr::defer(dr_close_lake(lake))
  data <- data.frame(
    id = bit64::as.integer64(c(
      "9007199254740993",
      "9007199254740995"
    ))
  )
  definition <- dr_contract(columns = c(id = "integer64"), key = "id")
  result <- dr_write_data(
    lake,
    data,
    "large_identifiers",
    contract = definition
  )
  expect_equal(result$status, "published")
  output <- dr_read_release(lake, "large_identifiers")
  expect_equal(as.character(output$id), as.character(data$id))
  expect_s3_class(output$id, "integer64")
  dr_expect_quality(dr_validate(output, definition))
  expect_equal(
    dr_profile_data(dr_read_release(lake, "large_identifiers", lazy = TRUE)),
    dr_profile_data(data)
  )
})

test_that("logical function vectors and formulas have the same row evidence", {
  data <- data.frame(amount = c(10, -1, NA))
  formula <- dr_run_quality(dr_quality_rule("positive", ~ amount > 0), data)
  fun <- dr_run_quality(
    dr_quality_rule("positive", function(x) x$amount > 0),
    data
  )
  expect_equal(fun, formula)
  expect_equal(fun$n_failed, 2)
  expect_equal(fun$n_total, 3)
  aggregate <- dr_run_quality(
    dr_quality_rule("aggregate", function(x) TRUE),
    data
  )
  expect_equal(aggregate$n_total, 1)
  unknown <- dr_run_quality(dr_quality_rule("aggregate", function(x) NA), data)
  expect_equal(unknown$status, "failed")
  expect_equal(unknown$n_failed, 1)
  expect_equal(
    dr_run_quality(dr_quality_rule("rows", ~TRUE), data)$n_total,
    3
  )
})

test_that("wrong quality outputs and empty evidence never pass", {
  data <- data.frame(id = 1:3)
  expect_error(
    dr_run_quality(dr_quality_rule("length", function(x) c(TRUE, FALSE)), data),
    "one per row"
  )
  expect_error(
    dr_run_quality(dr_quality_rule("type", function(x) 1), data),
    "logical values"
  )
  expect_error(
    dr_run_quality(
      dr_quality_rule("matrix", function(x) matrix(TRUE, 3, 1)),
      data
    ),
    "logical values"
  )
  expect_error(dr_quality_counts(0.5, 1), "Invalid quality counts")
  expect_error(dr_quality_counts("0", "1"), "Invalid quality counts")
  expect_error(dr_quality_counts(2, 1), "Invalid quality counts")
  expect_error(dr_quality_rule("bad", ~TRUE, threshold = "0"), "between")
  empty <- dr_run_quality(
    dr_quality_rule("empty", function(x) logical()),
    data[0, , drop = FALSE]
  )
  expect_equal(empty$status, "not_checked")
  definition <- dr_contract(
    columns = c(id = "integer"),
    rules = list(
      dr_quality_rule("broken", function(x) numeric())
    )
  )
  evidence <- dr_validate(data, definition, keep_errors = TRUE)
  expect_false(quality_ok(evidence))
  expect_match(
    conditionMessage(dr_quality_errors(evidence)$broken),
    "logical values"
  )
})

test_that("lazy formulas and native row functions agree", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  data <- data.frame(amount = c(10, -1, NA))
  DBI::dbWriteTable(con, "amounts", data)
  lazy <- dplyr::tbl(con, "amounts")
  expect_equal(
    dr_run_quality(dr_quality_rule("positive", ~ amount > 0), lazy),
    dr_run_quality(dr_quality_rule("positive", function(x) x$amount > 0), data)
  )
  DBI::dbExecute(con, "DELETE FROM amounts")
  expect_equal(
    dr_run_quality(dr_quality_rule("empty", ~ amount > 0), lazy)$status,
    "not_checked"
  )
})
