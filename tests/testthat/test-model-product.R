model_fixture <- function() {
  dm::dm(
    customers = data.frame(id = 1:2),
    policies = data.frame(policy = 1:2, id = 1:2, amount = c(10, 20))
  ) |>
    dm::dm_add_pk(customers, id) |>
    dm::dm_add_pk(policies, policy) |>
    dm::dm_add_fk(policies, id, customers)
}

test_that("models reuse contracts and diagnose a named table", {
  skip_if_not_installed("dm")
  spec <- dr_product(
    "portfolio",
    model_fixture(),
    contracts = list(
      policies = dr_contract(
        columns = c(policy = "integer", id = "integer", amount = "numeric"),
        key = "policy",
        rules = list(positive = ~ amount >= 0)
      )
    )
  )
  good <- dr_trial(spec)
  expect_s3_class(dr_collect(good), "dm")
  bad <- dr_trial(
    spec,
    sources = list(
      policies = data.frame(policy = 1:2, id = 1:2, amount = c(-1, 20))
    )
  )
  expect_identical(bad$status, "blocked")
  expect_equal(dr_quality_rows(bad)$policy, 1L)
  expect_equal(dr_quality_rows(bad, "policies/positive")$amount, -1)
  expect_equal(dr_collect(dr_trial(spec))$policies$amount, c(10, 20))
  orphan <- dr_trial(spec, sources = list(customers = data.frame(id = 1L)))
  expect_identical(orphan$status, "blocked")
  expect_equal(tail(dr_quality(orphan)$status, 1), "failed")
})

test_that("model publication pins all members and rejects stale correction", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("dm")
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  initialized <- dr_open_lake(
    root,
    backend = Sys.getenv("DATARAFT_TEST_BACKEND", "duckdb")
  )
  dr_close_lake(initialized)
  spec <- dr_product("portfolio", model_fixture())
  first <- dr_publish(spec, to = root)
  second <- dr_publish(
    spec,
    to = root,
    previous = first,
    sources = list(
      policies = data.frame(policy = 1:2, id = 2:1, amount = c(30, 40))
    )
  )
  expect_equal(dr_collect(first)$policies$amount, c(10, 20))
  expect_equal(dr_collect(second)$policies$amount, c(30, 40))
  expect_equal(
    dr_collect(dr_trial(dr_product(
      "selected",
      first,
      table = "policies"
    )))$amount,
    c(10, 20)
  )
  error <- tryCatch(
    dr_publish(spec, to = root, previous = first),
    error = identity
  )
  expect_s3_class(error, "dr_publication_conflict")
  blocked <- dr_publish(
    spec,
    to = root,
    sources = list(customers = data.frame(id = 1L)),
    stop_on_failure = FALSE
  )
  expect_identical(blocked$status, "blocked")
  lake <- dr_open_lake(root)
  withr::defer(dr_close_lake(lake))
  expect_equal(dr_read_release(lake, "portfolio")$policies$amount, c(30, 40))
  expect_equal(nrow(dr_releases(lake, "portfolio")), 2L)
  expect_equal(
    all(
      dr_quality(
        lake,
        asset = "portfolio",
        release = first$release_id
      )$status %in%
        c("passed", "unvalidated")
    ),
    TRUE
  )
})

test_that("a failed multi-table transaction leaves no partial release", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("dm")
  skip_if_not_installed("duckdb")
  lake <- dr_open_lake(
    withr::local_tempdir(),
    backend = Sys.getenv("DATARAFT_TEST_BACKEND", "duckdb")
  )
  withr::defer(dr_close_lake(lake))
  spec <- dr_product("portfolio", model_fixture())
  first <- dr_publish(spec, to = lake)
  before <- dr_releases(lake)
  real_insert <- insert_meta
  local_family_bindings(insert_meta = function(lake, name, values) {
    if (name == "releases" && identical(values$asset, "portfolio.policies")) {
      stop("injected failure")
    }
    real_insert(lake, name, values)
  })
  error <- tryCatch(
    dr_publish(spec, to = lake, previous = first),
    error = identity
  )
  expect_match(conditionMessage(error), "injected failure")
  expect_equal(dr_releases(lake), before)
  expect_equal(dr_collect(first)$policies$amount, c(10, 20))
})

test_that("table publication rejects a stale prior result", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  initialized <- dr_open_lake(
    root,
    backend = Sys.getenv("DATARAFT_TEST_BACKEND", "duckdb")
  )
  dr_close_lake(initialized)
  spec <- dr_product("orders", data.frame(id = 1L))
  first <- dr_publish(spec, to = root)
  second <- dr_publish(
    spec,
    to = root,
    previous = first,
    data = data.frame(id = 2L)
  )
  error <- tryCatch(
    dr_publish(spec, to = root, previous = first),
    error = identity
  )
  expect_s3_class(error$result$error, "dr_publication_conflict")
  expect_equal(dr_collect(second)$id, 2L)
})

test_that("model member names and established contracts cannot be bypassed", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("dm")
  skip_if_not_installed("duckdb")
  lake <- dr_open_lake(
    withr::local_tempdir(),
    backend = Sys.getenv("DATARAFT_TEST_BACKEND", "duckdb")
  )
  withr::defer(dr_close_lake(lake))
  spec <- dr_product(
    "portfolio",
    model_fixture(),
    contracts = list(
      customers = dr_contract(columns = c(id = "integer"), key = "id")
    )
  )
  first <- dr_publish(spec, to = lake)
  error <- tryCatch(
    dr_publish(dr_product("portfolio", model_fixture()), to = lake),
    error = identity
  )
  expect_match(conditionMessage(error), "Keep the explicit contract")
  error <- tryCatch(
    dr_write_data(lake, data.frame(id = 9L), "portfolio.customers"),
    error = identity
  )
  expect_match(conditionMessage(error), "model product")
  expect_equal(dr_collect(first)$customers$id, 1:2)
})

test_that("nested member selections reuse the active publication connection", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("dm")
  skip_if_not_installed("duckdb")
  root <- withr::local_tempdir()
  first <- dr_publish(dr_product("portfolio", model_fixture()), to = root)
  lake <- dr_open_lake(root)
  withr::defer(dr_close_lake(lake))
  real_connect <- dr_connect_lake
  local_family_bindings(dr_connect_lake = function(
    config,
    read_only = config$read_only
  ) {
    if (isTRUE(read_only)) {
      stop("Unexpected second read-only attachment")
    }
    real_connect(config, read_only = read_only)
  })
  selected <- dr_product("summary", first, table = "policies") |>
    dr_add_lookup(dr_product("lookup", first, table = "customers"), by = "id")
  out <- dr_publish(selected, to = lake)
  expect_equal(dr_collect(out)$amount, c(10, 20))
})

test_that("model lookups use table names without an extra wrapper product", {
  skip_if_not_installed("dm")
  checked <- dr_trial(dr_product("portfolio", model_fixture()))
  selected <- dr_product("summary", checked, table = "policies") |>
    dr_add_lookup(checked, table = "customers", by = "id")
  expect_equal(dr_collect(dr_trial(selected))$amount, c(10, 20))
  expect_equal(
    sort(names(delivery_aliases(selected))),
    c("customers", "summary")
  )
})

test_that("a model never silently chooses a metric grain", {
  skip_if_not_installed("dataraft.metrics")
  skip_if_not_installed("dm")
  result <- dr_trial(dr_product("portfolio", model_fixture()))
  error <- tryCatch(
    dr_measure(
      result,
      metrics = dr_metric_set("portfolio", count = dplyr::n())
    ),
    error = identity
  )
  expect_match(conditionMessage(error), "Choose a reporting table")
})
