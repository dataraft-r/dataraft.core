test_that("preparation is stored on the product and shares its execution path", {
  spec <- dr_product("orders") |> dr_add_quality(~ amount >= 0)
  preparation <- dr_recipe() |> dr_step_mutate(amount = amount * 2)
  product <- spec |> dr_add_recipe(preparation)
  expect_identical(class(product), "dr_product")
  expect_null(product$product)
  expect_identical(dr_extract_recipe(product), preparation)
  expect_equal(
    dr_collect(dr_run(product, data = data.frame(amount = 10)))$amount,
    20
  )
  changed <- dr_update_recipe(
    product,
    dr_recipe() |> dr_step_mutate(amount = amount + 1)
  )
  expect_equal(
    dr_collect(dr_run(changed, data = data.frame(amount = 10)))$amount,
    11
  )
  expect_equal(
    dr_collect(dr_run(
      dr_remove_recipe(product),
      data = data.frame(amount = 10)
    ))$amount,
    10
  )
  expect_length(spec$transforms, 0L)
})

test_that("the legacy modular spelling builds a product without nested workflow state", {
  reads <- 0L
  old <- dr_workflow() |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = amount * 2)) |>
    dr_add_source(function() {
      reads <<- reads + 1L
      data.frame(amount = 10)
    }) |>
    dr_add_product(dr_product("orders"))
  expect_identical(class(old), "dr_product")
  expect_null(old$product)
  validated <- dr_validate(old)
  expect_identical(attr(validated, "dr_validated"), TRUE)
  attr(validated, "dr_validated") <- NULL
  expect_identical(validated, old)
  expect_identical(reads, 0L)
  expect_equal(dr_collect(dr_run(old))$amount, 20)
  expect_identical(reads, 1L)
})

test_that("direct products preserve dry-run gates and lazy recipe execution", {
  writes <- 0L
  local_adapter_method(
    "dr_write_target",
    "review2_target",
    function(target, data, context, ...) {
      writes <<- writes + 1L
      list(type = "test")
    }
  )
  local_adapter_method(
    "dr_check_component",
    "review2_target",
    function(x, ...) invisible(x)
  )
  product <- dr_product("orders", data.frame(amount = c(10, -1))) |>
    dr_add_quality(~ amount >= 0) |>
    dr_set_target(structure(list(), class = "review2_target"))
  blocked <- dr_run(product, write = FALSE, stop_on_failure = FALSE)
  expect_identical(blocked$status, "blocked")
  expect_identical(writes, 0L)
  corrected <- dr_run(product, data = data.frame(amount = 2), write = FALSE)
  expect_equal(dr_collect(corrected)$amount, 2)
  expect_identical(writes, 0L)
  dr_run(product, data = data.frame(amount = 2))
  expect_identical(writes, 1L)
})
