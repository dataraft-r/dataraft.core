test_that("corrections update shared product references and lookup inputs lazily", {
  calls <- 0L
  old <- dr_product("rates", function() stop("old source ran"))
  left <- dr_product("left", data.frame(id = 1L)) |>
    dr_add_lookup(old, by = "id")
  right <- dr_product("right", old)
  root <- dr_product("root") |> dr_add_source(left) |> dr_add_source(right)
  fresh <- dr_product("rates", function() {
    calls <<- calls + 1L
    data.frame(id = 1L, rate = 2)
  })
  corrected <- dr_replace_sources(root, rates = fresh)
  expect_equal(calls, 0L)
  expect_identical(root$sources$right$sources$rates, old)
  expect_identical(corrected$sources$right$sources$rates, fresh)
  lookup <- product_sources(corrected$sources$left)
  expect_identical(lookup[[2]], fresh)
  corrected <- corrected |> dr_add_transform(function(data) data$left)
  expect_equal(dr_collect(dr_run(corrected))$rate, 2)
  expect_equal(calls, 1L)
})

test_that("root slots explicitly replace results while other pinned inputs persist", {
  accepted <- dr_run(dr_product("orders", data.frame(id = 1L)))
  newer <- dr_run(dr_product("orders", data.frame(id = 2L)))
  root <- dr_product("root") |>
    dr_add_source(accepted, name = "current") |>
    dr_add_source(accepted, name = "historical")
  changed <- dr_replace_sources(root, current = newer)
  expect_identical(changed$sources$historical, root$sources$historical)
  expect_equal(dr_read_source(changed$sources$current)$id, 2L)
  expect_snapshot(error = TRUE, dr_replace_sources(root, orders = newer))
})

test_that("invalid and overlapping source selectors fail before execution", {
  leaf <- dr_product("leaf", data.frame(id = 1L))
  branch <- dr_product("branch", leaf)
  root <- dr_product("root", branch)
  expect_snapshot(error = TRUE, dr_replace_sources(root, missing = leaf))
  expect_snapshot(
    error = TRUE,
    dr_replace_sources(root, leaf = leaf, leaf = leaf)
  )
  expect_snapshot(error = TRUE, dr_replace_sources(root, leaf))
  expect_snapshot(
    error = TRUE,
    dr_replace_sources(
      root,
      branch = dr_product("branch", data.frame(id = 1L)),
      leaf = leaf
    )
  )
  ambiguous <- root |> dr_add_source(data.frame(id = 1L), name = "leaf")
  expect_snapshot(error = TRUE, dr_replace_sources(ambiguous, leaf = leaf))
  cyclic <- branch
  cyclic$sources <- list(root = root)
  expect_snapshot(error = TRUE, dr_replace_sources(root, branch = cyclic))
  conflict <- dr_product("root") |>
    dr_add_source(branch) |>
    dr_add_source(dr_product("leaf", data.frame(id = 2L)), name = "other")
  expect_snapshot(error = TRUE, dr_replace_sources(conflict, branch = branch))
})

test_that("managed dbt bindings change without reading a database or writing files", {
  skip_if_not_installed("dataraft.dbt")
  skip_if_not_installed("dataraft.lake")
  root <- withr::local_tempdir()
  config <- dr_lake_config(
    dr_registry_duckdb(file.path(root, "lake.db")),
    dr_storage_local(file.path(root, "data")),
    landing = file.path(root, "landing"),
    backend = "duckdb"
  )
  result <- run_result("run-1", "published", "release-1")
  result$asset <- "orders"
  result$output_config <- config
  corrected <- result
  corrected$run_id <- "run-2"
  corrected$release_id <- "release-2"
  project <- dr_dbt_project(
    file.path(root, "absent"),
    lake = config,
    sources = list(orders = result, customers = result)
  )
  changed <- dr_replace_sources(project, orders = corrected)
  expect_equal(changed$source_groups$inputs$orders$release_id, "release-2")
  expect_identical(
    changed$source_groups$inputs$customers,
    project$source_groups$inputs$customers
  )
  expect_equal(list.files(root), character())
  project <- dr_dbt_sources(project, list(orders = result), name = "historical")
  expect_snapshot(error = TRUE, dr_replace_sources(project, orders = corrected))
  changed <- dr_replace_sources(project, inputs.orders = corrected)
  expect_equal(changed$source_groups$historical$orders$release_id, "release-1")
  expect_snapshot(
    error = TRUE,
    dr_replace_sources(
      project,
      inputs.orders = corrected,
      historical.orders = NULL
    )
  )
  expect_snapshot(
    error = TRUE,
    dr_replace_sources(changed, unknown = corrected)
  )
})

test_that("corrected definitions reuse existing targets caching", {
  skip_if_not_installed("dataraft.adapters")
  skip_if_not_installed("targets")
  root <- withr::local_tempdir()
  withr::local_dir(root)
  script <- function(amount) {
    writeLines(
      c(
        "library(dataraft)",
        "orders_definition <- dr_product('orders', data.frame(amount = 1))",
        "total_definition <- dr_product('total', orders_definition)",
        paste0(
          "fresh_definition <- dr_product('orders', data.frame(amount = ",
          amount,
          "))"
        ),
        "total_definition <- dr_replace_sources(total_definition, orders = fresh_definition)",
        "unrelated_definition <- dr_product('unrelated', data.frame(id = 1L))",
        "dr_as_targets(list(total_definition, unrelated_definition), evidence = 'evidence')"
      ),
      "_targets.R"
    )
  }
  script(2)
  targets::tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(nrow(dr_run_history("evidence")), 3L)
  unrelated_run <- targets::tar_read(unrelated)$run_id
  script(3)
  targets::tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(dr_collect(targets::tar_read(total))$amount, 3)
  expect_equal(nrow(dr_run_history("evidence")), 5L)
  expect_equal(targets::tar_read(unrelated)$run_id, unrelated_run)
})

test_that("correcting a product input retains its transformations and quality gates", {
  orders <- dr_product("orders", data.frame(amount = 1)) |>
    dplyr::mutate(amount = amount * 2) |>
    dr_add_quality(~ amount > 0)
  totals <- dr_product("totals", orders)
  corrected <- dr_replace_sources(totals, orders = data.frame(amount = 3))
  expect_equal(dr_collect(dr_run(corrected))$amount, 6)
  failed <- dr_replace_sources(totals, orders = data.frame(amount = -3))
  upstream <- dr_run(failed$sources$orders, stop_on_failure = FALSE)
  expect_equal(upstream$status, "blocked")
  expect_equal(dr_run(failed, stop_on_failure = FALSE)$status, "error")
  multiple <- dr_product("multiple") |>
    dr_add_source(orders) |>
    dr_add_source(data.frame(id = 1L), name = "other")
  expect_snapshot(
    error = TRUE,
    dr_replace_sources(
      dr_product("root", multiple),
      multiple = data.frame(id = 2L)
    )
  )
})

test_that("corrections retain exact immutable references in unselected slots", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  config <- dr_lake_config(
    dr_registry_duckdb(file.path(root, "lake.db")),
    dr_storage_local(file.path(root, "data")),
    landing = file.path(root, "landing"),
    backend = "duckdb"
  )
  old <- run_result("run-1", "published", "release-1")
  old$asset <- "orders"
  old$output_config <- config
  fresh <- old
  fresh$run_id <- "run-2"
  fresh$release_id <- "release-2"
  definition <- dr_product("report") |>
    dr_add_source(old, name = "current") |>
    dr_add_source(old, name = "historical")
  corrected <- dr_replace_sources(definition, current = fresh)
  expect_identical(corrected$sources$historical, definition$sources$historical)
  expect_equal(corrected$sources$current$release_id, "release-2")
  expect_equal(list.files(root), character())
})
