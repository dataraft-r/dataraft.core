test_that("deliveries retain meaningful names and original definitions", {
  orders <- dr_product(
    "orders",
    data.frame(amount = 10),
    source_name = "delivery"
  ) |>
    dr_add_transform(~ transform(.x, amount = amount * 2)) |>
    dr_add_quality(~ amount > 0)
  expect_named(dr_product("orders", data.frame(id = 1))$sources, "orders")
  expect_named(dr_product("summary", orders)$sources, "orders")
  expect_equal(
    dr_collect(dr_run(orders, data = data.frame(amount = 20)))$amount,
    40
  )
  expect_equal(dr_collect(dr_run(orders))$amount, 20)
  expect_named(orders$sources, "delivery")
  rejected <- dr_run(
    orders,
    data = data.frame(amount = -1),
    stop_on_failure = FALSE
  )
  expect_identical(rejected$status, "blocked")
  expect_equal(
    dr_collect(dr_run(
      dr_product("x", data.frame(id = 1)),
      sources = list(x = data.frame(id = 2))
    ))$id,
    2
  )
})

test_that("shared graph corrections read one source and preserve upstream gates", {
  reads <- 0L
  rates <- dr_product("rates", data.frame(id = 1L, rate = 1)) |>
    dr_add_quality(~ rate > 0)
  left <- dr_product("left", data.frame(id = 1L)) |>
    dr_add_lookup(rates, by = "id")
  right <- dr_product("right", rates)
  report <- dr_product("report") |>
    dr_add_source(left) |>
    dr_add_source(right) |>
    dr_add_transform(~ .x$left)
  fresh <- function() {
    reads <<- reads + 1L
    data.frame(id = 1L, rate = 2)
  }
  result <- dr_run(report, sources = list(rates = fresh))
  expect_equal(dr_collect(result)$rate, 2)
  expect_equal(reads, 1L)
  expect_equal(dr_collect(dr_run(report))$rate, 1)
  blocked <- dr_run(
    right,
    data = data.frame(id = 1L, rate = -1),
    stop_on_failure = FALSE
  )
  expect_identical(dr_status(blocked)$success, FALSE)
})

test_that("correction mistakes fail before any reader or writer", {
  skip_if_not_installed("dataraft.lake")
  reads <- 0L
  orders <- dr_product("orders", function() {
    reads <<- reads + 1L
    data.frame(id = 1L)
  })
  unknown <- tryCatch(
    dr_run(orders, sources = list(typo = data.frame(id = 2L))),
    error = identity
  )
  expect_match(
    conditionMessage(unknown),
    "Available names: orders",
    fixed = TRUE
  )
  both <- tryCatch(
    dr_run(
      orders,
      data = data.frame(id = 2L),
      sources = list(orders = data.frame(id = 3L))
    ),
    error = identity
  )
  expect_match(conditionMessage(both), "not both", fixed = TRUE)
  destination <- file.path(withr::local_tempdir(), "unopened")
  unknown <- tryCatch(
    dr_publish(
      orders,
      to = destination,
      sources = list(typo = data.frame(id = 2L))
    ),
    error = identity
  )
  expect_match(
    conditionMessage(unknown),
    "Available names: orders",
    fixed = TRUE
  )
  expect_identical(reads, 0L)
  expect_identical(dir.exists(destination), FALSE)
})

test_that("stored destinations apply only to a root and can be overridden", {
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  child_path <- file.path(root, "child")
  parent_path <- file.path(root, "parent")
  child <- dr_product(
    "orders",
    data.frame(amount = 10),
    contract = c(amount = "numeric"),
    execution = dr_execution_config(to = child_path)
  )
  parent <- dr_product(
    "summary",
    child,
    contract = c(amount = "numeric"),
    execution = dr_execution_config(to = parent_path)
  )
  first <- dr_publish(parent)
  expect_identical(first$status, "published")
  expect_identical(dir.exists(child_path), FALSE)
  second <- dr_publish(parent, data = data.frame(amount = 20))
  expect_equal(dr_collect(first)$amount, 10)
  expect_equal(dr_collect(second)$amount, 20)
  expect_equal(
    dr_collect(dr_run(parent, execution = dr_execution_config()))$amount,
    10
  )
  expect_identical(dir.exists(child_path), FALSE)
  expect_identical(dr_run(child)$status, "published")
})

test_that("ingestion uses a stored connection-free destination", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  root <- file.path(withr::local_tempdir(), "raw")
  orders <- dr_product(
    "orders",
    data.frame(amount = 10),
    contract = c(amount = "numeric"),
    execution = dr_execution_config(to = root)
  )
  accepted <- dr_ingest(orders)
  expect_identical(accepted$outputs$schema, "raw")
  expect_equal(dr_collect(accepted)$amount, 10)
  lake <- dr_open_lake(root)
  withr::defer(dr_close_lake(lake))
  error <- tryCatch(
    dr_product("orders", execution = dr_execution_config(to = lake)),
    error = identity
  )
  expect_match(
    conditionMessage(error),
    "cannot contain an open connection",
    fixed = TRUE
  )
})

test_that("targets embeds resolved defaults without activating child destinations", {
  skip_if_not_installed("dataraft.adapters")
  skip_if_not_installed("targets")
  path <- file.path(withr::local_tempdir(), "unused")
  child <- dr_product(
    "orders",
    data.frame(id = 1L),
    execution = dr_execution_config(to = path)
  )
  parent <- dr_product("summary", child)
  graph <- dr_as_targets(parent)
  root_command <- graph[[2L]]$command$expr[[1L]]
  child_command <- graph[[1L]]$command$expr[[1L]]
  expect_null(attr(child_command[[2L]], "dr_execution_config"))
  expect_null(child_command[[2L]]$target)
  expect_equal(dr_collect(eval(child_command))$id, 1L)
  expect_identical(dir.exists(path), FALSE)
  configured <- dr_product(
    "summary",
    child,
    contract = c(amount = "numeric"),
    execution = dr_execution_config(to = path)
  )
  changed_command <- dr_as_targets(configured)[[2L]]$command$expr[[1L]]
  expect_identical(identical(root_command, changed_command), FALSE)
  expect_s3_class(changed_command[[2L]]$target, "dr_lake_target")
})

test_that("managed dbt correction validation runs before commands", {
  skip_if_not_installed("dataraft.dbt")
  skip_if_not_installed("dataraft.lake")
  calls <- 0L
  local_family_bindings(dr_dbt_build = function(project, ...) {
    calls <<- calls + 1L
    project
  })
  root <- withr::local_tempdir()
  config <- dr_lake_config(path = file.path(root, "lake"))
  accepted <- run_result("run-1", "published", "release-1")
  accepted$asset <- "orders"
  accepted$output_config <- config
  corrected <- accepted
  corrected$release_id <- "release-2"
  project <- dr_dbt_project(
    file.path(root, "project"),
    lake = config,
    sources = list(orders = accepted)
  )
  result <- dr_run(project, sources = list(orders = corrected))
  expect_identical(result$source_groups[[1L]]$orders$release_id, "release-2")
  expect_identical(project$source_groups[[1L]]$orders$release_id, "release-1")
  error <- tryCatch(
    dr_run(project, sources = list(typo = corrected)),
    error = identity
  )
  expect_match(conditionMessage(error), "Available names:", fixed = TRUE)
  expect_identical(calls, 1L)
})

test_that("new deliveries retain every gate in a nested primary chain", {
  raw <- dr_product("raw", data.frame(amount = 2)) |>
    dr_add_quality(~ amount > 0)
  prepared <- dr_product("prepared", raw) |>
    dr_add_transform(~ transform(.x, amount = abs(amount)))
  report <- dr_product("report", prepared)
  original <- report
  bad <- data.frame(amount = -2)
  expect_identical(
    dr_run(report, data = bad, stop_on_failure = FALSE)$status,
    "error"
  )
  expect_identical(
    dr_run(
      report,
      sources = list(prepared = bad),
      stop_on_failure = FALSE
    )$status,
    "error"
  )
  aliased <- dr_product("report", prepared, source_name = "delivery")
  expect_identical(
    dr_run(
      aliased,
      sources = list(delivery = bad),
      stop_on_failure = FALSE
    )$status,
    "error"
  )
  expect_identical(report, original)
  expect_equal(dr_collect(dr_run(report))$amount, 2)
  expect_equal(
    dr_collect(dr_run(report, data = data.frame(amount = 3)))$amount,
    3
  )
  branches <- dr_product("branches") |>
    dr_add_source(raw) |>
    dr_add_source(raw, name = "other")
  ambiguous <- dr_product("report", dr_product("prepared", branches))
  error <- tryCatch(dr_run(ambiguous, data = bad), error = identity)
  expect_match(
    conditionMessage(error),
    "exactly one primary source at `branches`",
    fixed = TRUE
  )
})


test_that("a delivery updates shared leaf products across primary and lookup paths", {
  writes <- 0L
  local_family_bindings(dr_write_target.NULL = function(...) {
    writes <<- writes + 1L
    list(type = "memory")
  })
  raw <- dr_product("raw", data.frame(id = 1L, amount = 2)) |>
    dr_add_quality(~ amount > 0)
  prepared <- dr_product("prepared", raw) |> dplyr::select(id)
  shared <- dr_product("shared", prepared) |> dr_add_lookup(raw, by = "id")
  original <- shared
  result <- dr_run(shared, data = data.frame(id = 1L, amount = 3))
  expect_equal(dr_collect(result)$amount, 3)
  expect_identical(shared, original)
  writes <- 0L
  failed <- dr_run(
    shared,
    data = data.frame(id = 1L, amount = -3),
    stop_on_failure = FALSE
  )
  expect_identical(dr_status(failed)$success, FALSE)
  expect_identical(writes, 0L)
  expect_null(failed$outputs)
  expect_equal(dr_collect(dr_run(shared))$amount, 2)
})

test_that("printing and explaining a stored destination describe root writes", {
  skip_if_not_installed("dataraft.lake")
  path <- file.path(withr::local_tempdir(), "unopened")
  x <- dr_product(
    "orders",
    data.frame(id = 1L),
    execution = dr_execution_config(to = path, layer = "staging")
  )
  printed <- capture.output(print(x))
  explained <- capture.output(dr_explain(x))
  expect_match(
    paste(printed, collapse = "\n"),
    "Target: local lake (staging)",
    fixed = TRUE
  )
  expect_match(
    paste(printed, collapse = "\n"),
    "Stored execution (root only): quality = native",
    fixed = TRUE
  )
  expect_match(
    paste(explained, collapse = "\n"),
    "Publish: local lake",
    fixed = TRUE
  )
  expect_identical(dir.exists(path), FALSE)
  parent <- dr_product("summary", x)
  expect_match(
    paste(capture.output(dr_explain(parent)), collapse = "\n"),
    "Return: checked data",
    fixed = TRUE
  )
  expect_null(parent$target)
})
