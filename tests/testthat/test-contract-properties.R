test_that("generated Unicode numeric columns preserve null and threshold counts", {
  withr::local_seed(51921)
  for (iteration in seq_len(60)) {
    n <- sample(0:80, 1)
    values <- sample(c(NA_real_, -10:10), n, replace = TRUE)
    data <- setNames(data.frame(values), "Betrag_€")
    contract <- dr_contract(
      columns = c("Betrag_€" = "numeric"),
      allow_empty = TRUE,
      rules = list(~ `Betrag_€` >= 0)
    )
    evidence <- dr_validate(data, contract)
    expect_equal(
      evidence$n_failed[evidence$rule == "not_null:Betrag_€"],
      sum(is.na(values))
    )
    rule <- dr_run_quality(
      dr_quality_rule("nonnegative", ~ `Betrag_€` >= 0),
      data
    )
    expect_equal(rule$n_failed, sum(is.na(values) | values < 0))
    expect_equal(rule$n_total, n)
    expect_identical(evidence$status[evidence$rule == "types"], "passed")
  }
})

test_that("generated integer and double inputs follow the documented type relation", {
  withr::local_seed(59122)
  for (iteration in seq_len(40)) {
    integer <- sample(
      c(NA_integer_, -100L:100L),
      sample(0:30, 1),
      replace = TRUE
    )
    expect_identical(contract_type_matches(integer, "integer"), TRUE)
    expect_identical(contract_type_matches(integer, "numeric"), TRUE)
    expect_identical(
      contract_type_matches(as.double(integer), "integer"),
      FALSE
    )
    expect_identical(contract_type_matches(as.double(integer), "numeric"), TRUE)
  }
  for (zone in c("UTC", "Europe/Berlin", "America/New_York")) {
    dates <- as.POSIXct("2026-03-29", tz = zone) + runif(10, 0, 86400)
    expect_identical(contract_type_matches(dates, "POSIXct"), TRUE)
    expect_identical(contract_type_matches(dates, "numeric"), FALSE)
  }
})

test_that("integer64 remains distinct including values beyond double precision", {
  skip_if_not_installed("bit64")
  values <- bit64::as.integer64(c("9007199254740993", NA, "-9007199254740993"))
  expect_identical(contract_type_matches(values, "integer64"), TRUE)
  expect_identical(contract_type_matches(values, "numeric"), FALSE)
  expect_identical(contract_type_matches(values, "integer"), FALSE)
})

test_that("quality counts satisfy generated permutation and partition invariants", {
  skip_if_not_installed("hedgehog")
  withr::local_seed(59123)
  hedgehog::forall(
    hedgehog::gen.list(
      hedgehog::gen.element(c(NA_integer_, -10L:10L)),
      from = 0,
      to = 60
    ),
    function(values) {
      values <- as.integer(unlist(values))
      rule <- dr_quality_rule("positive", ~ amount > 0)
      counts <- function(x) {
        dr_run_quality(rule, data.frame(amount = x))$n_failed
      }
      expect_equal(counts(values), sum(is.na(values) | values <= 0))
      expect_equal(counts(rev(values)), counts(values))
      half <- seq_along(values) <= floor(length(values) / 2)
      expect_equal(counts(values), counts(values[half]) + counts(values[!half]))
    },
    tests = 100
  )
})
