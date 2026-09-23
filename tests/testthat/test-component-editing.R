test_that("contract slots are immutable and leave quality and sources intact", {
  old <- dr_contract("orders", columns = c(id = "integer"))
  revised <- dr_contract_update(old, version = "2")
  product <- dr_product("orders") |>
    dr_set_sources(delivery = function() stop("Do not execute sources")) |>
    dr_add_contract(old) |>
    dr_add_quality(~ id > 0)
  expect_identical(dr_add_contract(product, revised)$contract, revised)
  expect_identical(product$contract, old)
  removed <- dr_add_contract(product, NULL)
  expect_null(removed$contract)
  expect_identical(dr_add_contract(removed, NULL), removed)
  expect_identical(removed$quality, product$quality)
  expect_identical(removed$sources, product$sources)
  expect_identical(dr_add_contract(removed, revised)$contract, revised)
})

test_that("named source edits are deferred and preserve the original", {
  first <- function() stop("Never read first")
  second <- function() stop("Never read second")
  original <- dr_product("orders") |> dr_set_sources(delivery = first)
  changed <- dr_set_sources(original, delivery = second, other = first)
  expect_identical(original$sources$delivery, first)
  expect_identical(changed$sources$delivery, second)
  expect_identical(
    dr_set_sources(changed, other = NULL)$sources,
    list(delivery = second)
  )
  expect_length(dr_set_sources(changed, sources = NULL)$sources, 0L)
})

test_that("editing clears product preflight marks", {
  product <- dr_product("orders", data.frame(id = 1L)) |>
    dr_add_contract(c(id = "integer")) |>
    dr_validate()
  expect_identical(attr(product, "dr_validated"), TRUE)
  expect_null(attr(dr_add_contract(product, NULL), "dr_validated"))
  expect_null(attr(dr_set_sources(product, sources = NULL), "dr_validated"))
  expect_null(attr(
    dr_set_sources(product, delivery = data.frame(id = 2L)),
    "dr_validated"
  ))
})
