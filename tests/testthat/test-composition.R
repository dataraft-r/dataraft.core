test_that("minimal and progressively composed workflows use ordinary R objects", {
  data <- data.frame(id = 1:2, amount = c(10, 20))
  simple <- dr_product("orders") |> dr_add_source(data)
  expect_equal(dr_collect(dr_run(simple)), tibble::as_tibble(data))
  product <- simple |>
    dr_add_transform(~ transform(.x, amount = amount * 2), "double") |>
    dr_add_contract(list(id = integer(), amount = double())) |>
    dr_add_quality(list(positive = ~ amount > 0, total = function(data) {
      sum(data$amount) == 60
    }))
  result <- dr_run(product)
  expect_equal(dr_collect(result)$amount, c(20, 40))
  expect_equal(result$status, "completed")
  expect_equal(dr_status(result)$success, TRUE)
  expect_equal(dr_inspect(product)$sources$source_1$type, "data.frame")
  expect_equal(result$metadata$rows, 2L)
  expect_equal(result$metadata$schema, c(id = "integer", amount = "numeric"))
  expect_setequal(
    dr_quality(result)$rule,
    c(
      "schema",
      "types",
      "nonempty",
      "not_null:id",
      "not_null:amount",
      "positive",
      "total"
    )
  )
  expect_equal(tail(result$lifecycle$state, 1), "completed")
  expect_equal(result$inputs$rows, 2L)
  expect_identical(dr_collect(data), dplyr::collect(data))
})

test_that("definition, inspection and validation never call user functions", {
  calls <- 0
  product <- dr_product("orders") |>
    dr_add_source(function() {
      calls <<- calls + 1
      data.frame(id = 1L)
    }) |>
    dr_add_transform(function(data) {
      calls <<- calls + 1
      data
    })
  expect_equal(
    dr_plan(product)$step,
    c("read", "transform", "validate", "publish")
  )
  expect_equal(attr(dr_plan(product), "complete"), TRUE)
  expect_equal(dr_inspect(dr_validate(product))$status, "validated")
  expect_equal(
    dr_inspect(dr_validate(product) |> dr_add_quality(~ id > 0))$status,
    "defined"
  )
  expect_equal(calls, 0)
  expect_equal(dr_run(product)$status, "completed")
  expect_equal(calls, 2)
  expect_snapshot({
    print(product)
    dr_explain(product)
  })
})

test_that("invalid specifications fail before acquisition", {
  expect_snapshot(error = TRUE, dr_validate(dr_product("orders")))
})

test_that("bad transformations preserve an actionable condition and run evidence", {
  product <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1L)) |>
    dr_add_transform(function(data) list(id = 1L), "clean_orders")
  result <- dr_run(product, stop_on_failure = FALSE)
  expect_equal(result$status, "error")
  expect_match(
    conditionMessage(result$error),
    "clean_orders.*did not return a data frame"
  )
  expect_equal(tail(result$lifecycle$state, 1), "error")
  expect_snapshot(error = TRUE, dr_collect(result))
})

test_that("empty data and formula failures block a writer", {
  product <- dr_product("orders") |>
    dr_add_source(data.frame(amount = c(1, -1, NA))) |>
    dr_add_quality(~ amount >= 0)
  result <- dr_run(product, stop_on_failure = FALSE)
  expect_equal(result$status, "blocked")
  check <- result$quality[result$quality$rule == "amount >= 0", ]
  expect_equal(check$n_failed, 2)
  expect_equal(check$n_total, 3)
  expect_equal(
    dr_run(
      dr_product("empty") |> dr_add_source(data.frame(id = integer())),
      stop_on_failure = FALSE
    )$status,
    "blocked"
  )
})

test_that("contract prototypes, anonymous contracts and rule names normalize consistently", {
  contract <- dr_contract(
    columns = list(
      id = integer(),
      amount = double(),
      date = as.Date(character())
    )
  )
  product <- dr_product("orders") |> dr_add_contract(contract)
  expect_equal(product$contract$id, "orders.contract")
  expect_equal(
    unlist(contract$columns),
    c(id = "integer", amount = "numeric", date = "Date")
  )
  expect_snapshot(
    error = TRUE,
    dr_contract(columns = c(id = "integer", id = "numeric"))
  )
})

test_that("duplicate rule names across a contract and added checks fail preflight", {
  product <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1L)) |>
    dr_add_contract(dr_contract(
      "orders",
      columns = c(id = "integer"),
      rules = list(dr_quality_rule("positive", ~ id > 0))
    )) |>
    dr_add_quality(~ id >= 0, "positive")
  expect_snapshot(error = TRUE, dr_validate(product))
})

test_that("files normalize before execution and persist their original path", {
  root <- withr::local_tempdir()
  path <- file.path(root, "orders.csv")
  utils::write.csv(data.frame(id = 1:2), path, row.names = FALSE)
  product <- dr_product("orders") |> dr_add_source(path)
  expect_equal(
    product$sources$source_1$path,
    normalizePath(path, winslash = "/")
  )
  expect_equal(dr_collect(dr_run(product))$id, 1:2)
  expect_equal(dr_inspect(product)$sources$source_1$type, "file")
})

test_that("catalog delivery gets metadata without data rows or connections", {
  received <- NULL
  product <- dr_product("orders") |>
    dr_add_source(data.frame(
      id = 1:2,
      secret_value = c("private-a", "private-b")
    )) |>
    dr_add_catalog(function(metadata) received <<- metadata)
  result <- dr_run(product)
  expect_equal(received$rows, 2L)
  expect_equal(received$status, "completed")
  expect_equal(
    grepl("private-a", jsonlite::toJSON(received, auto_unbox = TRUE)),
    FALSE
  )
  expect_equal(result$warnings, character())
  product$catalogs <- list(function(metadata) stop("private credential"))
  expect_snapshot({
    result <- dr_run(product)
    print(result$status)
    print(result$warnings)
  })
  expect_equal(dr_collect(result)$id, 1:2)
})

test_that("execution warnings remain inspectable without entering metadata text", {
  detail <- "private source detail"
  product <- dr_product("orders") |>
    dr_add_source(function() {
      warning(detail)
      data.frame(id = 1L)
    })
  expect_snapshot({
    result <- dr_run(product)
    print(result$warnings)
  })
  expect_match(
    conditionMessage(result$warning_conditions[[1]]),
    "private source detail"
  )
  expect_equal(result$status, "completed")
  expect_equal(
    grepl(
      "private source detail",
      dataraft.core:::jencode(result$metadata),
      fixed = TRUE
    ),
    FALSE
  )
})
