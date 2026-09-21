test_that("fingerprints include lexical values but exclude unrelated bindings", {
  make <- function(threshold, unused = NULL) {
    force(threshold)
    ~ amount >= threshold
  }
  expect_identical(fingerprint(make(10)), fingerprint(make(10, "private")))
  expect_false(identical(fingerprint(make(10)), fingerprint(make(20))))
  expect_false(identical(
    fingerprint(rlang::as_quosure(make(10))),
    fingerprint(rlang::as_quosure(make(20)))
  ))
  expect_false(grepl("private", jencode(make(10, "private")), fixed = TRUE))
  expect_no_error(fingerprint(~ missing_data_column > 0))
})

test_that("function defaults and nested helpers participate in fingerprints", {
  make <- function(threshold) {
    force(threshold)
    helper <- function(x = threshold) x
    function(data) data$amount >= helper()
  }
  expect_identical(fingerprint(make(1)), fingerprint(make(1)))
  expect_false(identical(fingerprint(make(1)), fingerprint(make(2))))
})

test_that("reference contents affect signatures without exposing rows", {
  one <- dr_quality_reference(data.frame(id = "sensitive-one"), "id")
  two <- dr_quality_reference(data.frame(id = "sensitive-two"), "id")
  expect_false(identical(fingerprint(one), fingerprint(two)))
  expect_false(grepl("sensitive", jencode(one), fixed = TRUE))
  expect_identical(fingerprint(one), fingerprint(one))
})

test_that("cyclic and mutable captured state fails closed", {
  recursive <- function(x) recursive(x)
  expect_error(fingerprint(recursive), class = "dataraft_error_fingerprint")
  state <- new.env(parent = emptyenv())
  uses_state <- function(x) state$value
  expect_error(fingerprint(uses_state), class = "dataraft_error_fingerprint")
  env <- new.env(parent = baseenv())
  makeActiveBinding("secret", function() stop("must not evaluate"), env)
  formula <- rlang::new_formula(NULL, quote(value > secret), env)
  expect_error(fingerprint(formula), class = "dataraft_error_fingerprint")
})

test_that("captured atomic types and timezone attributes remain distinct", {
  make <- function(value) {
    force(value)
    function() value
  }
  expect_false(identical(fingerprint(make(1L)), fingerprint(make(1))))
  utc <- as.POSIXct("2026-01-01", tz = "UTC")
  berlin <- utc
  attr(berlin, "tzone") <- "Europe/Berlin"
  expect_false(identical(fingerprint(make(utc)), fingerprint(make(berlin))))
})

test_that("explicit environment pronouns retain capture identity even with a column", {
  make <- function(amount) {
    force(amount)
    ~ amount > .env$amount
  }
  expect_false(identical(
    canonical(make(1), masked = "amount"),
    canonical(make(2), masked = "amount")
  ))
  expect_error(
    fingerprint(~ base::get("threshold")),
    class = "dataraft_error_fingerprint"
  )
  expect_error(
    fingerprint(~ base::eval(quote(threshold))),
    class = "dataraft_error_fingerprint"
  )
  expect_error(
    fingerprint(~ .env[[column]]),
    class = "dataraft_error_fingerprint"
  )
})

test_that("declared contract columns mask ambient bindings", {
  make <- function(amount, threshold) {
    dr_contract(
      columns = c(amount = "numeric"),
      rules = list(~ amount >= threshold)
    )
  }
  expect_identical(fingerprint(make(new.env(), 1)), fingerprint(make(100, 1)))
  expect_false(identical(fingerprint(make(100, 1)), fingerprint(make(100, 2))))
  expect_no_error(fingerprint(function(data) data[, 1]))
  expect_error(
    fingerprint(function(x = base::get("threshold")) x),
    class = "dataraft_error_fingerprint"
  )
})

test_that("source descriptions label mutable factories without weakening rules", {
  state <- new.env(parent = emptyenv())
  source <- function() state$data
  product <- dr_product("runtime", source)
  description <- dr_inspect(product)$sources[[1]]
  expect_identical(description$fingerprintable, FALSE)
  expect_identical(description$dynamic, TRUE)
  expect_error(dr_inspect(source), class = "dataraft_error_fingerprint")
  expect_error(
    fingerprint(dr_quality_rule("mutable", source)),
    class = "dataraft_error_fingerprint"
  )
})
