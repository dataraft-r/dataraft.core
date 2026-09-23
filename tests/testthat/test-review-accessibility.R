test_that("missing optional packages identify an actionable dependency", {
  err <- tryCatch(
    need("dataraftMissingDependency", "Example storage"),
    error = identity
  )
  expect_s3_class(err, "dataraft_error_dependency")
  expect_s3_class(err, "dataraft_error")
  expect_match(conditionMessage(err), "Example storage", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    'install.packages("dataraftMissingDependency")',
    fixed = TRUE
  )
  expect_identical(err$package, "dataraftMissingDependency")
})

test_that("formula shortcuts preserve scope and explicit names", {
  cutoff <- 4
  short <- dr_quality_rule(~ amount >= cutoff)
  expect_identical(short$name, "amount >= cutoff")
  expect_identical(
    dr_run_quality(short, data.frame(amount = c(3, 5)))$n_failed,
    1
  )
  expect_identical(dr_quality_rule(check = ~ amount > 0)$name, "amount > 0")
  expect_identical(dr_quality_rule("positive", ~ amount > 0)$name, "positive")
  product <- dr_product("orders") |>
    dr_add_quality(~ amount > 0) |>
    dr_add_quality(~ amount > 0)
  expect_identical(
    vapply(product$quality, `[[`, character(1), "name"),
    c("amount > 0", "amount > 0.1")
  )
  expect_error(dr_add_quality(product, ~TRUE, name = "amount > 0"), "unique")
})

test_that("failed collection retains the result and local rule causes", {
  product <- dr_product("orders", data.frame(amount = 1)) |>
    dr_add_quality(~ amont >= 0)
  result <- dr_run(write = FALSE, stop_on_failure = FALSE, product)
  err <- tryCatch(dr_collect(result), error = identity)
  expect_s3_class(err, "dataraft_error_quality")
  expect_identical(err$result, result)
  causes <- dr_quality_errors(err)
  expect_named(causes, "amont >= 0")
  expect_match(conditionMessage(causes[[1]]), "amont")
  expect_match(conditionMessage(err), "result <- dr_last_failure", fixed = TRUE)
  expect_match(
    dr_quality_report(result)$message[
      dr_quality_report(result)$status == "error"
    ],
    "dr_quality_errors",
    fixed = TRUE
  )
})

test_that("raw rule exceptions stay out of exported quality evidence", {
  product <- dr_product("orders", data.frame(id = 1L)) |>
    dr_add_quality(function(data) stop("PRIVATE_PAYLOAD"), name = "broken")
  result <- dr_run(write = FALSE, stop_on_failure = FALSE, product)
  expect_match(
    conditionMessage(dr_quality_errors(result)$broken),
    "PRIVATE_PAYLOAD"
  )
  for (format in c("json", "html")) {
    path <- withr::local_tempfile()
    dr_quality_report(result, path, format = format)
    expect_false(any(grepl("PRIVATE_PAYLOAD", readLines(path))))
  }
  expect_false(grepl(
    "PRIVATE_PAYLOAD",
    conditionMessage(tryCatch(dr_collect(result), error = identity))
  ))
})

test_that("definitions explain how to obtain data without executing sources", {
  calls <- 0L
  product <- dr_product("orders", function() {
    calls <<- calls + 1L
    data.frame(id = 1L)
  })
  expect_error(
    dr_collect(product),
    "dr_run",
    class = "dataraft_error_definition"
  )
  expect_error(dr_collect(product), "dr_run")
  expect_identical(calls, 0L)
})

test_that("nested failures expose retained upstream rule errors", {
  raw <- dr_product("raw", data.frame(amount = 1)) |>
    dr_add_quality(~ amont > 0)
  result <- dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    dr_product("prepared", raw)
  )
  expect_match(conditionMessage(dr_quality_errors(result)[[1]]), "amont")
})

test_that("model failures retain member exceptions and collection evidence", {
  skip_if_not_installed("dm")
  tables <- dm::dm(orders = data.frame(amount = 1))
  contract <- dr_contract(
    columns = c(amount = "numeric"),
    rules = list(dr_quality_rule(~ amont > 0))
  )
  result <- dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    dr_product(
      "model",
      tables,
      contracts = list(orders = contract)
    )
  )
  err <- tryCatch(dr_collect(result), error = identity)
  expect_identical(err$result, result)
  expect_named(dr_quality_errors(err), "orders/amont > 0")
  expect_match(conditionMessage(dr_quality_errors(err)[[1]]), "amont")
})
