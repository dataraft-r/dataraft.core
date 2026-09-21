test_that("drafts infer types only and require an explicit review transition", {
  data <- data.frame(
    id = 1:2,
    amount = c(10, 20),
    date = as.Date(c("2026-01-01", "2026-01-02"))
  )
  draft <- dr_contract_from(data, "orders", "Analytics", "Orders", "One order")
  expect_s3_class(draft, "dr_contract_draft")
  expect_equal(draft$required, character())
  expect_equal(draft$key, character())
  expect_snapshot(error = TRUE, dr_validate(data, draft))
  draft$key <- "id"
  contract <- dr_contract_confirm(draft)
  expect_equal(
    contract$columns,
    list(id = "integer", amount = "numeric", date = "Date")
  )
  dr_expect_quality(dr_validate(data, contract))
  expect_equal(
    dr_contract_from(
      data.frame(x = factor("a")),
      "x",
      "Analytics",
      "Category",
      "One category"
    )$columns,
    list(x = "character")
  )
})

test_that("contract differences distinguish tightening from metadata and semantic changes", {
  old <- dr_contract(
    "orders",
    "1",
    "Analytics",
    "Orders",
    "One order",
    c(id = "integer", amount = "numeric"),
    required = "id"
  )
  new <- old
  new$version <- "2"
  new$required <- c("id", "amount")
  new$grain <- "One order at a date"
  new$owner <- "Finance"
  difference <- dr_contract_diff(old, new)
  expect_equal(difference$breaking[difference$field == "required"], TRUE)
  expect_equal(difference$breaking[difference$field == "owner"], FALSE)
  expect_equal(is.na(difference$breaking[difference$field == "grain"]), TRUE)
  expect_equal(nrow(dr_contract_diff(old, old)), 0L)
})
