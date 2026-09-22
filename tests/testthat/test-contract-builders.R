test_that("policy and metadata builders preserve constraints without duplication", {
  original <- dr_contract(columns = c(id = "integer", amount = "numeric"),
    key = "id", constraints = list(amount = list(min = 0)))
  changed <- original |>
    dr_contract_policy(allow_extra = TRUE, required = "id") |>
    dr_contract_meta(owner = "Risk", governance = list(classification = "internal"))
  expect_false(original$allow_extra)
  expect_true(changed$allow_extra)
  expect_identical(changed$owner, "Risk")
  expect_length(changed$rules, length(original$rules))
  expect_identical(attr(changed, "dr_anonymous"), TRUE)
  expect_error(dr_contract_policy(original, required = "missing"),
    class = "dataraft_error_contract")
})

test_that("write FALSE is the trial path for products and workflows", {
  product <- dr_product("trial", data.frame(id = 1L),
    contract = dr_contract(columns = c(id = "integer")))
  trial <- dr_trial(product)
  run <- dr_run(product, write = FALSE)
  expect_identical(run$status, trial$status)
  expect_equal(dr_collect(run), dr_collect(trial))
  expect_null(run$output_lake)
})

test_that("pointblank accepts the same action and threshold vocabulary", {
  rule <- dr_pointblank_checks("checks", function(data) data,
    action = "warn", threshold = 0.1)
  expect_identical(rule$action, "warn")
  expect_identical(rule$severity, "warning")
  expect_identical(rule$max_failure, 0.1)
  expect_error(dr_pointblank_checks("checks", identity,
    action = "warn", severity = "error"))
})
