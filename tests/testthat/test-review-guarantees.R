test_that("inferred schema is not presented as independent validation", {
  result <- dr_trial(dr_product("delivery", data.frame(value = c("a", NA))))
  expect_identical(result$validation_status, "unvalidated")
  expect_setequal(
    result$quality$rule[result$quality$status == "unvalidated"],
    c("schema", "types")
  )
  path <- withr::local_tempfile(fileext = ".html")
  dr_quality_report(result, path)
  expect_match(
    paste(readLines(path), collapse = ""),
    "Unvalidated: no declared contract"
  )
  declared <- dr_product("delivery", data.frame(value = "a")) |>
    dr_add_contract(dr_contract(columns = c(value = "character")))
  expect_identical(dr_trial(declared)$validation_status, "passed")
})

test_that("unstable quality callbacks need an explicit volatile declaration", {
  random <- dr_quality_rule(~ stats::runif(2) > 0)
  expect_error(
    dr_run_quality(random, data.frame(id = 1:2)),
    class = "dataraft_error_quality"
  )
  clock <- function(data) Sys.time() > as.POSIXct("2000-01-01")
  expect_error(
    dr_run_quality(dr_quality_rule("clock", clock), data.frame(id = 1)),
    class = "dataraft_error_quality"
  )
  state <- 0L
  changing <- function(data) {
    state <<- state + 1L
    state %% 2L == 0L
  }
  expect_error(
    dr_run_quality(dr_quality_rule("changing", changing), data.frame(id = 1)),
    class = "dataraft_error_quality"
  )
  allowed <- dr_quality_rule(~ stats::runif(2) > 0, volatile = TRUE)
  checks <- dr_run_quality(allowed, data.frame(id = 1:2))
  expect_identical(checks$status, "warning")
  expect_match(checks$message, "Volatile")
})

test_that("generic writers receive the data that actually passed the gate", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "delivery", data.frame(id = 1L))
  withr::local_options(dataraft.test.connection = con)
  check <- function(data) {
    DBI::dbExecute(
      getOption("dataraft.test.connection"),
      "UPDATE delivery SET id = -1"
    )
    data$id > 0
  }
  contract <- dr_contract(
    columns = c(id = "integer"),
    rules = list(dr_quality_rule("positive", check))
  )
  product <- dr_product("delivery", dplyr::tbl(con, "delivery")) |>
    dr_add_contract(contract)
  result <- dr_execute_target(NULL, product)
  expect_equal(result$data$id, 1L)
  expect_equal(DBI::dbReadTable(con, "delivery")$id, -1L)
})

test_that("write FALSE suppresses configured writers and evidence", {
  skip_if_not_installed("dataraft.adapters")
  path <- withr::local_tempfile(fileext = ".rds")
  product <- dr_product("delivery", data.frame(id = 1L)) |>
    dr_set_target(dataraft.adapters::dr_target_rds(path))
  result <- dr_run(product, write = FALSE)
  expect_identical(result$status, "completed")
  expect_false(file.exists(path))
})

test_that("backend protocols admit providers outside the DataRaft family", {
  dr_configure_target.example_target <- function(x, layer, ...) {
    x$layer <- layer
    x
  }
  target <- structure(list(), class = "example_target")
  expect_identical(
    dr_configure_target(target, layer = "approved")$layer,
    "approved"
  )
})

test_that("policy and metadata composition preserve independent definitions", {
  original <- dr_contract("orders", columns = c(id = "integer"), key = "id")
  composed <- original |>
    dr_contract_policy(allow_extra = TRUE) |>
    dr_contract_meta(owner = "Analytics", version = "2")
  expect_false(original$allow_extra)
  expect_true(composed$allow_extra)
  expect_identical(composed$version, "2")
  expect_identical(composed$owner, "Analytics")
  expect_identical(composed$key, "id")
  expect_error(
    dr_contract_policy(original, required = "missing"),
    class = "dataraft_error_contract"
  )
  expect_error(
    dr_contract_meta(original, undocumented = TRUE),
    class = "dataraft_error_contract"
  )
})


test_that("canonical quality accepts one policy vocabulary", {
  rule <- dr_quality(~ amount > 0, action = "warn", threshold = 0.1)
  expect_identical(
    dr_run_quality(rule, data.frame(amount = -1))$status,
    "warning"
  )
  expect_error(dr_quality(~TRUE, severity = "warning"), "action and threshold")
  expect_error(dr_quality(data.frame(), action = "warn"), "formula or check")
  # A column named after an RNG function is not an RNG call.
  expect_identical(
    dr_run_quality(dr_quality(~ sample > 0), data.frame(sample = 1))$status,
    "passed"
  )
})

test_that("pointblank volatility is explicit and labelled", {
  skip_if_not_installed("pointblank")
  expect_error(
    dr_run_quality(
      dr_quality(~ stats::runif(1) > 0, engine = "pointblank"),
      data.frame(id = 1)
    ),
    "volatile"
  )
  out <- dr_run_quality(
    dr_quality(~ stats::runif(1) >= 0, engine = "pointblank", volatile = TRUE),
    data.frame(id = 1)
  )
  expect_identical(out$engine, "pointblank:volatile")
  expect_identical(out$status, "warning")
})

test_that("products bind their first execution delivery without a workflow", {
  product <- dr_product(
    "orders",
    contract = dr_contract(columns = c(id = "integer"))
  )
  result <- dr_run(product, data = data.frame(id = 1L), write = FALSE)
  expect_identical(dr_collect(result)$id, 1L)
  expect_length(product$sources, 0L)
  expect_error(
    dr_run(
      product,
      data = data.frame(id = 1L),
      sources = list(orders = data.frame(id = 2L))
    ),
    "both data and sources"
  )
})
