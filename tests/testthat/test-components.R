test_that("DBI sources use parameters and close only factory-owned connections", {
  skip_if_not_installed("dataraft.adapters")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "orders", data.frame(id = 1:3))
  source <- dr_source_database(
    con,
    query = "SELECT * FROM orders WHERE id > ?",
    params = list(1L)
  )
  product <- dr_product("orders") |> dr_add_source(source)
  expect_equal(dr_collect(dr_run(product))$id, 2:3)
  expect_equal(DBI::dbIsValid(con), TRUE)
  expect_equal(
    dr_collect(dr_read_source(dr_source_database(
      con,
      table = DBI::Id(table = "orders")
    )))$id,
    1:3
  )
  factory_con <- NULL
  factory <- function() {
    factory_con <<- DBI::dbConnect(duckdb::duckdb())
    factory_con
  }
  source <- dr_source_database(factory, query = "SELECT 42 AS id")
  dr_check_component(source)
  expect_null(factory_con)
  expect_equal(dr_read_source(source)$id, 42L)
  expect_equal(DBI::dbIsValid(factory_con), FALSE)
})

test_that("SQL transforms work with ordinary data and reject missing infrastructure in preflight", {
  skip_if_not_installed("dataraft.adapters")
  skip_if_not_installed("duckdb")
  product <- dr_product("totals") |>
    dr_add_source(data.frame(amount = c(10, 20))) |>
    dr_add_transform(dr_sql_transform("SELECT sum(amount) AS total FROM data"))
  expect_equal(dr_collect(dr_run(product))$total, 30)
  local_family_bindings(need = function(package) {
    stop(paste("Install optional package:", package))
  })
  expect_snapshot(error = TRUE, dr_validate(product))
})

test_that("quality adapters fail closed on malformed evidence", {
  local_method <- function(generic, class, method) {
    table <- get(".__S3MethodsTable__.", envir = asNamespace("dataraft.core"))
    name <- paste(generic, class, sep = ".")
    assign(name, method, envir = table)
    withr::defer(rm(list = name, envir = table), envir = parent.frame())
  }
  rule <- dr_quality_rule("external", ~ id > 0)
  class(rule) <- c("external_quality", "dr_rule")
  local_method("dr_run_quality", "external_quality", function(rule, data, ...) {
    data.frame(status = "passed")
  })
  product <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1L)) |>
    dr_add_quality(rule)
  result <- dr_run(product, stop_on_failure = FALSE)
  expect_equal(result$status, "blocked")
  expect_equal(tail(result$quality$status, 1), "error")
})
