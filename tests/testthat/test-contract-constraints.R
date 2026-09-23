test_that("bounds, enums and nullability produce independent blocking evidence", {
  contract <- dr_contract(
    columns = c(amount = "numeric", status = "character")
  ) |>
    dataraft.core::dr_contract_policy(
      constraints = list(
        amount = list(min = 0, max = 100, nullable = TRUE),
        status = list(enum = c("open", "closed"))
      )
    )
  data <- data.frame(
    amount = c(NA, -1, 101, 50),
    status = c("open", "closed", "unknown", NA)
  )
  evidence <- dr_validate(data, contract)
  failures <- setNames(evidence$n_failed, evidence$rule)
  expect_equal(failures[["constraint:amount:min"]], 1)
  expect_equal(failures[["constraint:amount:max"]], 1)
  expect_equal(failures[["constraint:status:enum"]], 1)
  expect_equal(failures[["not_null:status"]], 1)
  expect_false("not_null:amount" %in% evidence$rule)
  expect_identical(contract$constraints$amount$min, 0)
  expect_false(identical(
    fingerprint(contract),
    fingerprint(
      dr_contract(columns = c(amount = "numeric", status = "character")) |>
        dataraft.core::dr_contract_policy(
          constraints = list(
            amount = list(min = 1, max = 100, nullable = TRUE),
            status = list(enum = c("open", "closed"))
          )
        )
    )
  ))
})

test_that("invalid column constraints are classed definition errors", {
  for (spec in list(
    list(min = 5, max = 1),
    list(enum = NA_real_),
    list(nullable = NA),
    list(timezone = "UTC"),
    list(precision = 1.5),
    list(currency = "EUR")
  )) {
    expect_error(
      dr_contract(columns = c(amount = "numeric")) |>
        dataraft.core::dr_contract_policy(constraints = list(amount = spec)),
      class = "dataraft_error_contract"
    )
  }
  expect_error(
    dr_contract(columns = c(id = "integer"), key = "id") |>
      dataraft.core::dr_contract_policy(
        constraints = list(id = list(nullable = TRUE))
      ),
    class = "dataraft_error_contract"
  )
})

test_that("timezone and precision constraints reject lossy representations", {
  contract <- dr_contract(columns = c(at = "POSIXct", amount = "numeric")) |>
    dataraft.core::dr_contract_policy(
      constraints = list(
        at = list(timezone = "UTC", precision = 0),
        amount = list(precision = 2)
      )
    )
  data <- data.frame(
    at = as.POSIXct("2026-01-01", tz = "UTC") + c(0, 0.5),
    amount = c(123456789.01, 123456789.012)
  )
  evidence <- dr_validate(data, contract)
  failures <- setNames(evidence$n_failed, evidence$rule)
  expect_equal(failures[["constraint:at:timezone"]], 0)
  expect_equal(failures[["constraint:at:precision"]], 1)
  expect_equal(failures[["constraint:amount:precision"]], 1)
  attr(data$at, "tzone") <- "Europe/Berlin"
  evidence <- dr_validate(data, contract)
  expect_equal(evidence$n_failed[evidence$rule == "constraint:at:timezone"], 2)
})

test_that("portable constraints agree on lazy tables and unsupported precision blocks", {
  skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  data <- data.frame(
    amount = c(NA, -1, 101, 50),
    status = c("open", "closed", "unknown", NA)
  )
  lazy <- dplyr::copy_to(con, data, "input")
  contract <- dr_contract(
    columns = c(amount = "numeric", status = "character")
  ) |>
    dataraft.core::dr_contract_policy(
      constraints = list(
        amount = list(min = 0, max = 100, nullable = TRUE),
        status = list(enum = c("open", "closed"))
      )
    )
  expect_equal(dr_validate(lazy, contract), dr_validate(data, contract))
  precise <- dr_contract(
    columns = c(amount = "numeric", status = "character")
  ) |>
    dataraft.core::dr_contract_policy(
      constraints = list(amount = list(precision = 2))
    )
  evidence <- dr_validate(lazy, precise, keep_errors = TRUE)
  expect_identical(
    evidence$status[evidence$rule == "constraint:amount:precision"],
    "error"
  )
  expect_s3_class(
    dr_quality_errors(evidence)[[1]],
    "dataraft_error_contract_constraint_backend"
  )
})

test_that("constraint parameters cannot be shadowed by input column names", {
  contract <- dr_contract(columns = c(value = "numeric", column = "numeric")) |>
    dataraft.core::dr_contract_policy(
      constraints = list(value = list(min = 5), column = list(max = 1))
    )
  evidence <- dr_validate(data.frame(value = 2, column = 2), contract)
  expect_equal(evidence$n_failed[evidence$rule == "constraint:value:min"], 1)
  expect_equal(evidence$n_failed[evidence$rule == "constraint:column:max"], 1)
})


test_that("replacing constraint policy rebuilds gates without losing custom rules", {
  original <- dr_contract(
    "amounts",
    c(amount = "numeric"),
    rules = list(dr_quality_rule("finite", ~ is.finite(amount)))
  ) |>
    dr_contract_policy(constraints = list(amount = list(min = 0)))
  revised <- original |>
    dr_contract_policy(constraints = list(amount = list(min = 10)))
  expect_identical(
    vapply(revised$rules, `[[`, character(1), "name"),
    c("finite", "constraint:amount:min")
  )
  evidence <- dr_validate(data.frame(amount = 5), revised)
  expect_identical(
    evidence$status[evidence$rule == "constraint:amount:min"],
    "failed"
  )
  expect_length(dr_contract_policy(revised, constraints = list())$rules, 1L)
  expect_length(dr_contract_update(revised, version = "2")$rules, 2L)
})
