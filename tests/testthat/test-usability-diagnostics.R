test_that("blocked results explain actual checks without counting rows twice", {
  definition <- dr_product("orders", data.frame(amount = c(1, -2, 3))) |>
    dr_add_contract(c(amount = "numeric")) |>
    dr_add_quality(list(amount = ~ amount >= 0))
  result <- dr_run(definition, stop_on_failure = FALSE)
  expect_equal(dr_status(result)$outcome, "blocked")
  expect_match(dr_status(result)$message, "orders is blocked", fixed = TRUE)
  expect_match(dr_status(result)$message, "1 of 3 checks failed", fixed = TRUE)
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "dr_quality_rows(result)", fixed = TRUE)
  expect_false(grepl(result$run_id, printed, fixed = TRUE))
  expect_false(grepl("release: NA", printed, fixed = TRUE))
  expect_snapshot(error = TRUE, dr_collect(result))
  report <- dr_quality_report(result)
  expect_s3_class(report, "tbl_df")
  expect_equal(report$n_failed[report$rule == "amount"], 1)
  expect_equal(report$n_total[report$rule == "amount"], 3)
  expect_false("data" %in% names(report))
})

test_that("execution summaries distinguish completion from publication and redact errors", {
  done <- dr_run(dr_product("orders", data.frame(id = 1L),
    contract = c(id = "integer")))
  expect_match(dr_status(done)$message, "completed", fixed = TRUE)
  expect_false(grepl("published", dr_status(done)$message, fixed = TRUE))
  missing <- run_result("secret-run-id", "missing")
  missing$error <- simpleError("token=SECRET and private source rows")
  expect_match(dr_status(missing)$message, "delivery is missing", fixed = TRUE)
  expect_false(grepl(
    "SECRET",
    paste(capture.output(print(missing)), collapse = "")
  ))
  expect_equal(dr_quality(missing)$status, "not_checked")
  expect_equal(
    dr_quality_report(quality_row("", "")[0, ])$status,
    "not_checked"
  )
})

test_that("measurement quality follows pinned evidence and preserves borrowed connections", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("dataraft.metrics")
  skip_if_not_installed("duckdb")
  f <- fixture()
  on.exit(fixture_cleanup(f))
  original <- dr_run(f$pipeline, f$lake)
  measured <- dr_measure(f$lake, metrics = list(reserve = reserve_metric()))
  # Explicit fixture reference also verifies the diagnostic reader independently.
  attr(measured[[1]], "dr_quality_reference") <- list(
    lake = f$lake,
    config = f$lake$config,
    asset = "risk.validated",
    release = original$release_id
  )
  f$write(transform(f$good, reserve = -1))
  blocked <- dr_run(f$pipeline, f$lake, stop_on_failure = FALSE)
  expect_equal(blocked$status, "blocked")
  checks <- dr_quality(measured)
  expect_equal(dr_quality(measured[[1]]), checks)
  expect_equal(dr_status(measured[[1]]), dr_status(measured))
  expect_equal(dr_lineage(measured[[1]]), dr_lineage(measured))
  expect_true(all(checks$status %in% c("passed", "warning")))
  expect_true(all(checks$.release == original$release_id))
  expect_true(DBI::dbIsValid(f$lake$con))
  dr_disconnect_lake(f$lake)
  expect_equal(dr_quality(measured), checks)
  f$lake <- dr_connect_lake(f$lake$config)
  expect_true(DBI::dbIsValid(f$lake$con))

  attr(measured[[1]], "dr_quality_reference")$release <- "different-release"
  expect_equal(dr_quality(measured)$status, "not_checked")
  attr(measured[[1]], "dr_quality_reference") <- NULL
  expect_equal(dr_quality_report(measured)$status, "not_checked")
})


test_that("large diagnoses stay concise without claiming allowed checks blocked", {
  checks <- dplyr::bind_rows(lapply(seq_len(5), function(i) {
    quality_row(paste0("rule_", i), "not_checked")
  }))
  result <- run_result("internal", "completed", quality = checks)
  result$asset <- "orders"
  text <- dr_status(result)$message
  expect_match(text, "5 checks requiring attention", fixed = TRUE)
  expect_match(text, "2 more", fixed = TRUE)
  expect_false(grepl("rule_4", text, fixed = TRUE))
  expect_false(grepl("blocking", text, fixed = TRUE))
})


test_that("default failed runs explain checks and retain inspectable evidence", {
  definition <- dr_product("orders", data.frame(amount = c(1, -2, 3))) |>
    dr_add_contract(c(amount = "numeric")) |>
    dr_add_quality(list(amount = ~ amount >= 0))
  expect_snapshot(error = TRUE, dr_run(definition))
  failure <- tryCatch(dr_run(definition), dr_run_failed = identity)
  expect_equal(failure$result$status, "blocked")
  expect_equal(
    dr_quality(failure$result)$n_failed[
      dr_quality(failure$result)$rule == "amount"
    ],
    1
  )

  broken <- dr_product("orders", data.frame(amount = 1)) |>
    dr_add_transform(function(data) stop("token=TOP_SECRET"))
  failure <- tryCatch(dr_run(broken), dr_run_failed = identity)
  expect_match(conditionMessage(failure), "orders failed", fixed = TRUE)
  expect_false(grepl("TOP_SECRET", conditionMessage(failure), fixed = TRUE))
  expect_match(
    conditionMessage(failure$result$error),
    "TOP_SECRET",
    fixed = TRUE
  )
  expect_s3_class(failure$parent, "dr_execution_cause")
})


test_that("nested failures surface upstream evidence without reexecuting sources", {
  raw <- dr_product("raw", data.frame(amount = -2)) |>
    dr_add_contract(c(amount = "numeric")) |>
    dr_add_quality(list(amount = ~ amount > 0))
  prepared <- dr_product("prepared", raw)
  report <- dr_product("report", prepared)
  result <- dr_run(report, stop_on_failure = FALSE)
  expect_false(dr_status(result)$success)
  expect_match(
    dr_status(result)$message,
    "Upstream: raw is blocked",
    fixed = TRUE
  )
  expect_match(dr_status(result)$message, "1 of 1 checks failed", fixed = TRUE)
  expect_equal(
    dr_quality(result)$n_failed[dr_quality(result)$rule == "amount"],
    1
  )
})


test_that("known definition failures retain safe reasons and actionable advice", {
  result <- run_result("internal", "error")
  result$asset <- "orders"
  result$error <- rlang::error_cnd(
    "dr_definition_changed",
    message = "private backend context token=SECRET",
    definition_id = "orders",
    definition_version = "1"
  )
  message <- dr_status(result)$message
  expect_match(
    message,
    "Definition changed without a version bump: orders 1",
    fixed = TRUE
  )
  expect_match(message, "new version before publishing", fixed = TRUE)
  expect_false(grepl("SECRET", message, fixed = TRUE))
  result$error <- rlang::error_cnd("dr_read_only", message = "private endpoint")
  expect_match(
    dr_status(result)$message,
    "Choose a writable target",
    fixed = TRUE
  )
  expect_false(grepl(
    "private endpoint",
    dr_status(result)$message,
    fixed = TRUE
  ))
})
