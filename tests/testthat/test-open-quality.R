test_that("quarantine partitions rows before the writer and preserves evidence", {
  product <- dr_product("orders", data.frame(amount = c(1, -1, NA))) |>
    dr_add_quality(~ amount >= 0, action = "quarantine")
  result <- dr_trial(product)
  expect_equal(dr_collect(result)$amount, 1)
  expect_equal(nrow(dr_quarantine_rows(result)), 2L)
  expect_true(any(result$quality$stage == "quarantine"))
  expect_equal(
    sum(result$quality$n_failed[result$quality$stage == "quarantine"]),
    2
  )
  broken <- dr_trial(dr_add_quality(
    dr_product("broken", data.frame(amount = 1)),
    ~ amont > 0,
    action = "quarantine"
  ))
  expect_false(broken$status %in% c("published", "completed"))
})

test_that("action and threshold reuse the existing quality gate", {
  x <- data.frame(amount = c(1, -1))
  expect_equal(
    dr_run_quality(dr_quality_rule(~ amount > 0, action = "warn"), x)$status,
    "warning"
  )
  expect_equal(
    dr_run_quality(dr_quality_rule(~ amount > 0, threshold = .5), x)$status,
    "passed"
  )
  expect_error(
    dr_quality_rule(~TRUE, action = "warn", severity = "error"),
    "action or severity"
  )
  expect_error(
    dr_add_quality(dr_product("x"), ~TRUE, threshold = 2),
    "threshold"
  )
})

test_that("unpartitioned quarantine never authorizes rejected rows", {
  rule <- dr_quality_rule(~ amount > 0, action = "quarantine", threshold = 1)
  expect_equal(dr_run_quality(rule, data.frame(amount = -1))$status, "failed")
})

test_that("plain console diagnostics remain readable with NO_COLOR", {
  withr::local_envvar(c(NO_COLOR = "1"))
  result <- dr_trial(dr_product("orders", data.frame(id = 1L)))
  output <- paste(capture.output(print(result)), collapse = "\n")
  expect_false(grepl("\033[", output, fixed = TRUE))
  expect_match(output, "completed")
})
