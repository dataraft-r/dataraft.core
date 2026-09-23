test_that("contract revisions retain identity and guarantee review checks", {
  old <- dr_contract("orders", columns = c(id = "integer"), key = "id")
  expect_identical(
    dr_update_contract(old, version = "2"),
    dr_contract_update(old, version = "2")
  )
  expect_error(dr_update_contract(old), class = "dataraft_error_contract")
  expect_error(
    dr_update_contract(old, old, version = "2"),
    class = "dataraft_error_contract"
  )
  expect_error(
    dr_update_contract(old, version = "2", remove = "id"),
    class = "dataraft_error_contract"
  )
})

test_that("contract slots work on products and workflow products without execution", {
  old <- dr_contract("orders", columns = c(id = "integer"))
  revised <- dr_update_contract(old, version = "2")
  product <- dr_product("orders") |>
    dr_add_source(function() stop("Source must remain deferred")) |>
    dr_add_contract(old) |>
    dr_add_quality(
      function(data) stop("Rule must remain deferred"),
      name = "check"
    )
  for (x in list(product, dr_workflow() |> dr_add_product(product))) {
    expect_identical(dr_extract_contract(x), old)
    expect_identical(
      dr_extract_contract(dr_update_contract(x, revised)),
      revised
    )
    expect_identical(dr_extract_contract(x), old)
    removed <- dr_remove_contract(x)
    expect_identical(dr_remove_contract(removed), removed)
    expect_error(
      dr_extract_contract(removed),
      class = "dataraft_error_definition"
    )
    expect_error(
      dr_update_contract(removed, revised),
      class = "dataraft_error_definition"
    )
    expect_identical(
      dr_extract_contract(dr_add_contract(removed, revised)),
      revised
    )
    edited_product <- if (inherits(removed, "dr_product_workflow")) {
      removed$product
    } else {
      removed
    }
    expect_identical(edited_product$quality, product$quality)
    expect_identical(edited_product$sources, product$sources)
  }
  expect_error(dr_update_contract(product), class = "dataraft_error_contract")
  expect_error(
    dr_update_contract(product, revised, version = "3"),
    class = "rlib_error_dots_nonempty"
  )
  expect_null(dr_remove_contract(dr_workflow())$contract)
})

test_that("named source edits are deferred, immutable and unambiguous", {
  first <- function() stop("Never read first")
  second <- function() stop("Never read second")
  for (empty in list(dr_product("orders"), dr_workflow())) {
    x <- dr_add_source(empty, first, name = "delivery")
    expect_identical(dr_extract_source(x), first)
    expect_identical(dr_extract_source(dr_update_source(x, second)), second)
    expect_identical(dr_extract_source(x), first)
    multiple <- dr_add_source(x, second, name = "other")
    expect_error(
      dr_extract_source(multiple),
      class = "dataraft_error_definition"
    )
    expect_error(
      dr_update_source(multiple, first),
      class = "dataraft_error_definition"
    )
    expect_error(
      dr_remove_source(multiple),
      class = "dataraft_error_definition"
    )
    expect_error(
      dr_update_source(x, second, name = "missing"),
      class = "dataraft_error_definition"
    )
    expect_identical(
      dr_extract_source(dr_remove_source(multiple, "other")),
      first
    )
    removed <- dr_remove_source(x)
    expect_identical(dr_remove_source(removed, "delivery"), removed)
    expect_error(
      dr_extract_source(removed),
      class = "dataraft_error_definition"
    )
  }
})

test_that("legacy builders expose the same primary source slots as products", {
  product <- dr_product("orders") |>
    dr_set_sources(delivery = data.frame(id = 1L))
  flow <- dr_workflow() |> dr_add_product(product)
  changed <- dr_set_sources(flow, delivery = data.frame(id = 2L))
  expect_equal(changed$sources$delivery$id, 2L)
  expect_length(dr_set_sources(flow, delivery = NULL)$sources, 0L)
  expect_equal(product$sources$delivery$id, 1L)
})

test_that("editing clears product preflight marks", {
  product <- dr_product("orders", data.frame(id = 1L)) |>
    dr_add_contract(c(id = "integer")) |>
    dr_validate()
  expect_identical(attr(product, "dr_validated"), TRUE)
  expect_null(attr(dr_remove_contract(product), "dr_validated"))
  expect_null(attr(dr_remove_source(product), "dr_validated"))
  expect_null(attr(
    dr_update_source(product, data.frame(id = 2L)),
    "dr_validated"
  ))
})
