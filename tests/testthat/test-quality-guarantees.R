test_that("inferred schemas are explicitly unvalidated and remain inspectable", {
  product <- dr_product("observed", data.frame(amount = c(1, NA_real_)))
  result <- dr_trial(product)
  expect_identical(result$status, "unvalidated")
  expect_identical(dr_collect(result)$amount, c(1, NA_real_))
  checks <- dr_quality_report(result)
  expect_identical(checks$status[checks$rule == "schema"], "unvalidated")
  expect_identical(checks$status[checks$rule == "types"], "unvalidated")
  expect_false(quality_ok(result$quality))
  path <- withr::local_tempfile(fileext = ".html")
  dr_quality_report(result, path)
  expect_match(paste(readLines(path), collapse = ""), "Unvalidated")
})

test_that("a declared contract supplies actual validation evidence", {
  product <- dr_product("declared", data.frame(amount = 1:3)) |>
    dr_add_contract(dr_contract(columns = c(amount = "integer")))
  result <- dr_trial(product)
  expect_identical(result$status, "completed")
  expect_true(quality_ok(result$quality))
})

test_that("known changing quality calls require explicit volatility", {
  for (predicate in list(~ runif(1) > 0, ~ stats::rnorm(1) > 0,
                         ~ Sys.time() > 0)) {
    rule <- dr_quality_rule("changing", predicate)
    expect_error(dr_run_quality(rule, data.frame(x = 1)),
                 class = "dataraft_error_quality_volatile")
  }
  draw <- stats::runif
  alias <- dr_quality_rule("alias", ~ draw(1) > 0)
  expect_error(dr_run_quality(alias, data.frame(x = 1)),
               class = "dataraft_error_quality_volatile")
  clock <- function() Sys.time()
  rule <- dr_quality_rule("indirect", ~ clock() > 0)
  expect_error(dr_run_quality(rule, data.frame(x = 1)),
               class = "dataraft_error_quality_volatile")
})

test_that("declared volatile checks never authorize publication", {
  rule <- dr_quality_rule("clock", ~ Sys.time() > 0, volatile = TRUE)
  evidence <- dr_run_quality(rule, data.frame(x = 1))
  expect_identical(evidence$status, "unvalidated")
  expect_false(quality_ok(evidence))
  product <- dr_product("volatile", data.frame(x = 1L)) |>
    dr_add_contract(dr_contract(columns = c(x = "integer"))) |>
    dr_add_quality(~ Sys.time() > 0, volatile = TRUE)
  result <- dr_trial(product)
  expect_identical(result$status, "unvalidated")
  expect_equal(nrow(dr_collect(result)), 1L)
})

test_that("generic lazy execution checks and retains one materialization", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "delivery", data.frame(id = 1L))
  check <- function(data) {
    DBI::dbExecute(con, "UPDATE delivery SET id = -1")
    data$id > 0
  }
  product <- dr_product("snapshot", dplyr::tbl(con, "delivery")) |>
    dr_add_contract(dr_contract(columns = c(id = "integer"),
      rules = list(dr_quality_rule("positive", check))))
  written <- NULL
  local_adapter_method("dr_check_component", "snapshot_target",
    function(x, ...) invisible(x))
  local_adapter_method("dr_write_target", "snapshot_target",
    function(target, data, context, ...) {
      written <<- data
      list(backend = "snapshot")
    })
  product <- dr_set_target(product, structure(list(), class = "snapshot_target"))
  result <- dr_run(product)
  expect_identical(result$status, "published")
  expect_identical(written$id, 1L)
  expect_identical(dr_collect(result)$id, 1L)
  expect_equal(DBI::dbReadTable(con, "delivery")$id, -1)
})


test_that("unvalidated products never invoke a configured writer", {
  calls <- 0L
  local_adapter_method("dr_check_component", "unvalidated_target",
    function(x, ...) invisible(x))
  local_adapter_method("dr_write_target", "unvalidated_target",
    function(target, data, context, ...) {
      calls <<- calls + 1L
      list(backend = "unexpected")
    })
  product <- dr_product("unvalidated", data.frame(id = 1L)) |>
    dr_set_target(structure(list(), class = "unvalidated_target"))
  result <- dr_run(product, stop_on_failure = FALSE)
  expect_identical(result$status, "unvalidated")
  expect_identical(calls, 0L)
})
