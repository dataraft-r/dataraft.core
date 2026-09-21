test_that("reviewed removal and aggregation replace guarantees explicitly", {
  old <- dr_contract(
    "orders",
    grain = "One order",
    columns = c(id = "integer", amount = "numeric"),
    key = "id",
    rules = list(positive = ~ amount > 0),
    column_metadata = list(id = list(description = "Identifier"))
  )
  new <- dr_contract_update(
    old,
    id = "totals",
    remove = "id",
    columns = c(total = "numeric"),
    grain = "One total",
    required = "total",
    key = character(),
    rules = list(positive = ~ total > 0)
  )
  expect_identical(new$key, character())
  expect_identical(new$required, "total")
  expect_null(new$column_metadata)
  expect_identical(new$grain, "One total")
  expect_identical(names(new$columns), c("amount", "total"))
  expect_identical(all.vars(new$rules[[1]]$check), "total")
  changed <- dr_contract_update(
    old,
    version = "2",
    columns = c(id = "character"),
    key = "id",
    rules = old$rules
  )
  expect_identical(changed$columns$id, "character")
})

test_that("unsafe inheritance and unchanged identity have actionable errors", {
  old <- dr_contract(
    "orders",
    grain = "One order",
    columns = c(id = "integer", amount = "numeric"),
    key = "id",
    rules = list(positive = ~ amount > 0)
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, columns = c(extra = "numeric"))
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", grain = "One month")
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(
      old,
      version = "2",
      grain = "One month",
      key = character()
    )
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", remove = "amount")
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", remove = "amount", required = "id")
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", columns = c(id = "character"))
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", columns = c(amount = "integer"))
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", remove = "missing")
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(old, version = "2", typo = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    dr_contract_update(
      old,
      version = "2",
      remove = "amount",
      columns = c(amount = "numeric")
    )
  )
})
