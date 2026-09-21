test_that("native states have consistent outcome categories", {
  states <- c("completed", "published", "cached", "blocked", "error", "missing")
  outcomes <- vapply(
    states,
    function(state) {
      inspected <- dr_status(run_result("run", state))
      expect_equal(inspected$status, state)
      inspected$outcome
    },
    character(1)
  )
  expect_equal(
    unname(outcomes),
    c("succeeded", "succeeded", "succeeded", "blocked", "failed", "blocked")
  )
  dbt <- structure(
    list(
      success = FALSE,
      results = tibble::tibble(
        unique_id = letters[1:5],
        status = c("success", "warn", "fail", "error", "skipped")
      )
    ),
    class = "dr_dbt_result"
  )
  expect_equal(
    dr_status(dbt)$outcome,
    c("succeeded", "succeeded", "blocked", "failed", "skipped", "failed")
  )
  dbt$success <- TRUE
  dbt$results <- dbt$results[0, ]
  expect_equal(nrow(dr_status(dbt)), 0L)
  expect_type(dr_status(dbt)$outcome, "character")
})

test_that("result lineage uses exact recorded input evidence", {
  accepted <- dr_run(dr_product("orders", data.frame(id = 1L)))
  latest <- dr_run(dr_product("orders", data.frame(id = 2L)))
  result <- dr_run(dr_product("report", accepted))
  edges <- dr_lineage(result)
  expect_equal(edges$from_id, "orders")
  expect_equal(edges$from_version, accepted$run_id)
  expect_equal(edges$to_id, "report")
  expect_equal(edges$to_version, result$run_id)
  expect_equal(edges$run_id, result$run_id)
  expect_equal(nrow(dr_lineage(result, "unrelated")), 0L)
  expect_equal(dr_lineage(result, "report"), edges)
  expect_equal(nrow(dr_lineage(accepted)), 0L)
  expect_identical(names(dr_lineage(accepted)), names(edges))
  expect_equal(nrow(dr_lineage(run_result("failed", "error"))), 0L)
})

test_that("recorded lake lineage is retained without a live connection", {
  result <- run_result("run", "published", "release")
  result$metadata <- list(
    lineage = tibble::tibble(
      run_id = "run",
      from_id = "source",
      from_version = "old-release",
      to_id = "product",
      to_version = "release",
      relation = "published_from"
    )
  )
  expect_identical(dr_lineage(result), result$metadata$lineage)
})

test_that("measurement diagnostics retain exact manifests without inventing checks", {
  skip_if_not_installed("dataraft.metrics")
  values <- tibble::tibble(value = 10)
  attr(values, "dr_manifest") <- list(
    metric = "total",
    metric_version = "1",
    product = "orders",
    release_id = "original-release",
    input_quality = "passed"
  )
  measurements <- list(total = values)
  attr(measurements, "dr_set_metadata") <- list(total = list(metric = "total"))
  attr(measurements, "dr_set_hash") <- measurement_set_hash(measurements)
  class(measurements) <- c("dr_measurement_set", "list")
  expect_equal(dr_status(measurements)$outcome, "succeeded")
  expect_equal(dr_status(measurements)$release_id, "original-release")
  expect_equal(dr_lineage(measurements)$from_version, "original-release")
  expect_equal(dr_lineage(measurements)$to_id, "total")
  expect_equal(dr_lineage(measurements)$run_id, NA_character_)
  expect_equal(dr_quality(measurements)$status, "not_checked")
  expect_equal(dr_quality(measurements)$.release, "original-release")
})
