test_that("run evidence is durable and excludes rows and executable definitions", {
  path <- withr::local_tempdir()
  secret <- "fixture-secret-never-persist"
  result <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1:2, private = secret)) |>
    dr_add_transform(function(data) data) |>
    dr_run(evidence = path)
  saved <- dr_read_run(path, result$run_id)
  expect_equal(saved$status, "completed")
  expect_equal(dr_run_history(path)$run_id, result$run_id)
  text <- paste(readLines(result$evidence), collapse = "\n")
  expect_false(grepl(secret, text, fixed = TRUE))
  expect_false(grepl('"body"|"formals"|"data"', text))
  expect_equal(saved$schema$id, "integer")
  expect_equal(length(saved$inputs), 1L)
  expect_equal(saved$inputs[[1]]$rows, 2)
})

test_that("failed and blocked runs retain incidents without raw errors", {
  path <- withr::local_tempdir()
  blocked <- dr_product("orders") |>
    dr_add_source(data.frame(id = 1:2)) |>
    dr_add_quality(~ id < 0, "impossible") |>
    dr_run(evidence = path, stop_on_failure = FALSE)
  failed <- dr_product("broken") |>
    dr_add_source(function() stop("secret-row-value")) |>
    dr_run(evidence = path, stop_on_failure = FALSE)
  expect_equal(dr_read_run(path, blocked$run_id)$status, "blocked")
  expect_true("impossible" %in% dr_incidents(path)$rule)
  expect_equal(
    dr_incidents(blocked)$run_id,
    rep(blocked$run_id, nrow(dr_incidents(blocked)))
  )
  expect_equal(dr_read_run(path, failed$run_id)$status, "error")
  expect_false(grepl(
    "secret-row-value",
    paste(readLines(failed$evidence), collapse = "")
  ))
})

test_that("outbox retries fresh matching destinations and never resends success", {
  path <- withr::local_tempdir()
  calls <- 0L
  received <- NULL
  callback <- function(metadata) {
    calls <<- calls + 1L
    stop("server error with secret")
  }
  expect_warning(
    result <- dr_product("orders") |>
      dr_add_source(data.frame(id = 1L)) |>
      dr_add_catalog(callback, name = "business") |>
      dr_run(evidence = path),
    "delivery failed"
  )
  expect_equal(result$status, "completed")
  expect_equal(dr_read_run(path, result$run_id)$deliveries$business$attempts, 1)
  expect_equal(dr_run_history(path)$pending_catalogs, 1L)
  dr_retry_catalogs(path, list(other = function(x) stop("must not run")))
  expect_equal(dr_run_history(path)$pending_catalogs, 1L)
  dr_retry_catalogs(
    path,
    list(business = function(metadata) received <<- metadata)
  )
  expect_equal(received$run_id, result$run_id)
  expect_equal(dr_run_history(path)$pending_catalogs, 0L)
  expect_equal(dr_read_run(path, result$run_id)$deliveries$business$attempts, 2)
  dr_retry_catalogs(path, list(business = function(x) stop("must not run")))
  expect_equal(dr_read_run(path, result$run_id)$deliveries$business$attempts, 2)
  expect_equal(calls, 1L)
})

test_that("evidence failures are visible without changing execution status", {
  path <- withr::local_tempfile()
  writeLines("not a directory", path)
  expect_warning(
    result <- dr_product("orders") |>
      dr_add_source(data.frame(id = 1L)) |>
      dr_run(evidence = path),
    "evidence"
  )
  expect_equal(result$status, "completed")
  expect_s3_class(result$evidence_error, "condition")
  expect_null(result$evidence)
  expect_equal(dr_collect(result)$id, 1L)
})

test_that("evidence allowlist removes URL credentials and query secrets", {
  result <- structure(
    list(
      run_id = "fixture",
      status = "error",
      asset = "orders",
      inputs = list(
        source = list(
          type = "API",
          snapshot = "3776207205136740581",
          path = "https://user:password@example.test/data?token=private",
          query = "SELECT secret",
          request = list(password = "secret")
        )
      ),
      error = simpleError("private"),
      metadata = list()
    ),
    class = "dr_run_result"
  )
  record <- safe_run_evidence(result)
  expect_equal(record$inputs[[1]]$source$path, "https://example.test/data")
  expect_identical(record$inputs[[1]]$source$snapshot, "3776207205136740581")
  expect_null(record$inputs[[1]]$source$query)
  expect_null(record$inputs[[1]]$source$request)
  expect_false(grepl("private|password|SELECT", jsonlite::toJSON(record)))
  expect_error(dr_read_run(tempdir(), "../invalid"), "Invalid run")
})

test_that("lake registry inputs retain source identity in durable lineage", {
  skip_if_not_installed("dataraft.catalog")
  result <- structure(
    list(
      run_id = "lake_fixture",
      asset = "orders",
      status = "published",
      started_at = now(),
      finished_at = now(),
      inputs = tibble::tibble(
        source = "orders.delivery",
        source_version = "delivery-v2",
        landed_path = "https://user:password@example.test/archive?token=private",
        original_name = "orders.csv",
        release_id = "release_fixture",
        fingerprint = "content-fingerprint"
      ),
      metadata = list(schema = c(id = "integer"))
    ),
    class = "dr_run_result"
  )
  path <- withr::local_tempdir()
  save_run_evidence(safe_run_evidence(result), path)
  record <- dr_read_run(path, result$run_id)
  input <- record$inputs[[1L]]
  expect_equal(input$source$id, "orders.delivery")
  expect_equal(input$source$version, "delivery-v2")
  expect_equal(input$source$path, "https://example.test/archive")
  expect_equal(input$release_id, "release_fixture")
  expect_equal(input$fingerprint, "content-fingerprint")
  events <- openlineage_events(
    dr_catalog_openlineage("https://example.test/lineage"),
    record
  )
  expect_equal(events[[2L]]$inputs[[1L]]$name, "orders.delivery")
  expect_false(grepl("password|private", jsonlite::toJSON(events)))
})
