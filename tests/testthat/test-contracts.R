test_that("preferred quality terms preserve legacy gate semantics and reject ambiguity", {
  predicate <- ~ amount >= 0
  legacy <- dr_quality_rule(
    "positive",
    predicate,
    severity = "warning",
    max_failure = .1
  )
  preferred <- dr_quality_rule(
    "positive",
    predicate,
    action = "warn",
    threshold = .1
  )
  expect_identical(preferred$severity, legacy$severity)
  expect_identical(preferred$max_failure, legacy$max_failure)
  expect_identical(preferred$check, legacy$check)
  expect_identical(preferred$action, "warn")
  expect_error(
    dr_quality_rule(
      "positive",
      predicate,
      action = "block",
      severity = "error"
    ),
    "Use action or severity, not both",
    class = "dataraft_error"
  )
  expect_error(
    dr_quality_rule("positive", predicate, threshold = 0, max_failure = 0),
    "Use threshold or max_failure, not both",
    class = "dataraft_error"
  )
  quarantined <- dr_quality_rule(
    "positive",
    predicate,
    action = "quarantine",
    threshold = .1
  )
  expect_identical(quarantined$action, "quarantine")
  expect_identical(quarantined$severity, "error")
})
