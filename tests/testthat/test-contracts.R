test_that("quality specifications use action and threshold without aliases", {
  predicate <- ~ amount >= 0
  rule <- dr_quality_rule(
    "positive",
    predicate,
    action = "warn",
    threshold = .1
  )
  expect_identical(rule$action, "warn")
  expect_identical(rule$threshold, .1)
  expect_identical(rule$check, predicate)
  expect_error(
    dr_quality_rule("positive", predicate, severity = "warning"),
    "unused argument"
  )
  expect_error(
    dr_quality_rule("positive", predicate, max_failure = 0),
    "unused argument"
  )
  expect_identical(
    dr_quality_rule("positive", predicate, action = "quarantine")$action,
    "quarantine"
  )
})
