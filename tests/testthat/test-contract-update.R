test_that("contract composition preserves identity choices and optional columns", {
  old <- dr_contract(
    "orders",
    columns = c(id = "integer"),
    key = "id",
    owner = "Finance",
    operator = "Platform",
    column_metadata = list(id = list(description = "Order identifier"))
  )
  original <- old
  new <- dr_contract_update(
    old,
    id = "enriched",
    columns = list(channel = character())
  )
  expect_identical(new$columns, list(id = "integer", channel = "character"))
  expect_identical(new$required, "id")
  expect_identical(new$key, "id")
  expect_identical(new$owner, "Finance")
  expect_identical(new$operator, "Platform")
  expect_identical(new$column_metadata, old$column_metadata)
  expect_identical(old, original)
  revised <- dr_contract_update(old, version = "2", description = "Revised")
  expect_identical(revised$id, old$id)
  expect_identical(revised$version, "2")
  expect_identical(revised$description, "Revised")
})

test_that("contracts and product rules use the same normalization without evaluation", {
  checks <- list(nonnegative = ~ amount >= 0, opaque = function(data) {
    stop("Do not execute while defining")
  })
  definition <- dr_contract(
    "amounts",
    columns = c(amount = "numeric"),
    rules = checks
  )
  p <- dr_add_quality(dr_product("amounts"), checks)
  expect_identical(definition$rules, p$quality)
  expect_identical(
    dr_contract(columns = c(amount = "numeric"), rules = ~ amount > 0)$rules[[
      1
    ]]$name,
    "amount > 0"
  )
  reused <- dr_quality_rule("old_name", ~ amount > 0, engine = "native")
  normalized <- dr_contract(
    columns = c(amount = "numeric"),
    rules = list(positive = reused)
  )$rules[[1]]
  expect_identical(normalized$name, "positive")
  expect_identical(normalized$engine_explicit, TRUE)
  expect_identical(definition$rules[[1]]$engine_explicit, FALSE)
  expect_identical(
    dr_contract_update(definition, id = "copy")$rules,
    definition$rules
  )
})
