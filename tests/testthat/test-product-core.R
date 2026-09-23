test_that("products normalize contracts and metadata through one class", {
  x <- dr_product("orders", contract = c(id = "integer"))
  y <- dr_product("orders") |>
    dr_add_contract(dr_contract(
      columns = c(id = "integer"),
      owner = "Analytics",
      description = "Order records"
    ))
  expect_identical(class(x), "dr_product")
  expect_identical(class(y), class(x))
  expect_identical(dr_inspect(y)$owner, "Analytics")
  expect_identical(dr_inspect(y)$description, "Order records")
  expect_null(x$contract$max_age_hours)
})

test_that("named sources accumulate and replacement is explicit", {
  a <- data.frame(id = 1:2)
  b <- data.frame(id = 1:2, value = c(10, 20))
  x <- dr_product("joined") |>
    dr_add_source(a, "keys") |>
    dr_add_source(b, "values")
  expect_named(x$sources, c("keys", "values"))
  expect_error(dr_add_source(x, b, "keys"), "replace = TRUE")
  replaced <- dr_add_source(x, data.frame(id = 2L), "keys", replace = TRUE)
  x <- x |>
    dr_add_transform(function(data) merge(data$keys, data$values, by = "id"))
  expect_equal(dr_collect(dr_run(x))$value, c(10, 20))
  expect_equal(replaced$sources$keys$id, 2L)
  expect_error(dr_run(replaced), class = "dr_run_failed")
  failed <- dr_run(replaced, stop_on_failure = FALSE)
  expect_match(conditionMessage(failed$error), "Combine multiple sources")
})

test_that("shared dependencies execute once and invalid graphs fail before IO", {
  calls <- 0L
  shared <- dr_product("shared") |>
    dr_add_source(function() {
      calls <<- calls + 1L
      data.frame(id = 1:2)
    })
  left <- dr_product("left") |> dr_add_source(shared)
  right <- dr_product("right") |> dr_add_source(shared)
  joined <- dr_product("joined") |>
    dr_add_source(left) |>
    dr_add_source(right) |>
    dr_add_transform(function(data) merge(data$left, data$right, by = "id"))
  expect_equal(dr_collect(dr_run(joined))$id, 1:2)
  expect_equal(calls, 1L)
  cycle <- shared |> dr_add_source(left)
  expect_error(dr_run(cycle), class = "dr_dependency_cycle")
  expect_equal(calls, 1L)
  alternate <- shared |> dr_add_transform(identity)
  conflict <- joined
  conflict$sources$right$sources$shared <- alternate
  expect_error(dr_run(conflict), class = "dr_dependency_conflict")
  expect_equal(calls, 1L)
})

test_that("sources are acquired once and DBI results are materialized before the gate", {
  skip_if_not_installed("dataraft.adapters")
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbWriteTable(con, "orders", data.frame(id = 1:3, amount = c(10, 20, 30)))
  calls <- 0L
  source <- function() {
    calls <<- calls + 1L
    dplyr::tbl(con, "orders")
  }
  transform <- function(data) dplyr::filter(data, amount > 10)
  lazy <- dr_product("orders") |>
    dr_add_source(source) |>
    dr_add_transform(transform) |>
    dr_add_quality(~ amount > 0)
  result <- dr_run(lazy)
  expect_s3_class(result$data, "data.frame")
  expect_identical(dr_inspect(result$data)$columns, c("id", "amount"))
  expect_equal(dr_collect(result)$id, 2:3)
  expect_equal(calls, 1L)
  expect_equal(result$inputs$hash_kind, "definition")
  ordinary <- dr_product("orders") |>
    dr_add_source(DBI::dbReadTable(con, "orders")) |>
    dr_add_transform(transform)
  expect_equal(dr_collect(dr_run(ordinary)), dr_collect(result))
  expect_true(DBI::dbIsValid(con))
  expect_true(dr_capabilities(dr_source_database(con, "orders"))$lazy)
  expect_error(
    dr_source_database(function() con, "orders", lazy = TRUE),
    "lazy = FALSE"
  )
})

test_that("capabilities use one stable shape and explain materialization", {
  expected <- c(
    "read",
    "write",
    "lazy",
    "transactions",
    "partition",
    "immutable"
  )
  expect_named(dr_capabilities(identity), expected)
  expect_true(is.na(dr_capabilities(identity)$lazy))
  expect_identical(dr_capabilities(NULL)$lazy, TRUE)
  expect_error(dr_component_capabilities(lazy = "yes"), "TRUE, FALSE or NA")
  x <- dr_product("orders") |> dr_add_source(data.frame(id = 1L))
  expect_named(dr_inspect(x)$sources, "source_1")
  expect_true("materializes" %in% names(dr_plan(x)))
  expect_output(dr_explain(x), "collect\\(\\) materializes")
})

test_that("multiple lake sources preserve original bytes and pinned lineage", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  lake <- dr_open_lake(file.path(root, "lake"))
  withr::defer(dr_close_lake(lake))
  a <- file.path(root, "keys.csv")
  b <- file.path(root, "values.csv")
  utils::write.csv(data.frame(id = 1:2), a, row.names = FALSE)
  utils::write.csv(
    data.frame(id = 1:2, amount = c(10, 20)),
    b,
    row.names = FALSE
  )
  reads <- 0L
  reader <- function(path) {
    reads <<- reads + 1L
    utils::read.csv(path)
  }
  x <- dr_product("joined") |>
    dr_add_source(a, "keys", reader = reader) |>
    dr_add_source(b, "values", reader = reader) |>
    dr_add_transform(function(data) merge(data$keys, data$values, by = "id")) |>
    dr_set_target(lake)
  result <- dr_run(x)
  expect_equal(reads, 2L)
  expect_equal(dr_collect(result)$amount, c(10, 20))
  originals <- result$inputs[
    result$inputs$original_name %in% c("keys.csv", "values.csv"),
  ]
  expect_equal(nrow(originals), 2L)
  expect_true(all(file.exists(originals$landed_path)))
  for (i in seq_len(nrow(originals))) {
    path <- file.path(root, originals$original_name[[i]])
    expect_identical(
      readBin(path, "raw", n = file.info(path)$size),
      readBin(originals$landed_path[[i]], "raw", n = file.info(path)$size)
    )
  }
  derived <- dr_product("copy") |>
    dr_add_source(dr_source_release(lake, "joined")) |>
    dr_set_target(lake)
  copied <- dr_run(derived)
  expect_true(result$release_id %in% copied$inputs$source_version)
  edges <- copied$metadata$lineage
  expect_true(result$release_id %in% edges$from_version)
})


test_that("writer candidate evidence is distinguished from the submitted batch", {
  local_adapter_method(
    "dr_write_target",
    "candidate_target",
    function(target, data, context, ...) {
      candidate <- rbind(data.frame(id = 1L), data)
      checks <- dr_validate(candidate, context$contract)
      if (!quality_ok(checks)) {
        abort(
          "Candidate rejected.",
          "dr_target_quality_failed",
          quality = checks
        )
      }
      list(
        type = "candidate",
        rows = nrow(candidate),
        written_rows = nrow(data),
        candidate_quality = checks
      )
    }
  )
  local_adapter_method(
    "dr_check_component",
    "candidate_target",
    function(x, ...) {
      invisible(x)
    }
  )
  x <- dr_product(
    "orders",
    contract = dr_contract(columns = c(id = "integer"), key = "id")
  ) |>
    dr_add_source(data.frame(id = 2L)) |>
    dr_set_target(structure(list(), class = "candidate_target"))
  result <- dr_run(x)
  expect_equal(result$metadata$rows, 2)
  expect_equal(result$metadata$submitted_rows, 1)
  expect_equal(dr_collect(result)$id, 2L)
  expect_equal(result$quality$n_total[result$quality$rule == "unique_key"], 2)
  failed <- dr_run(
    x |> dr_add_source(data.frame(id = 1L), "source_1", replace = TRUE),
    stop_on_failure = FALSE
  )
  expect_identical(failed$status, "blocked")
  expect_true(any(failed$quality$status == "failed"))
})
