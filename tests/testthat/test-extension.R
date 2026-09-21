test_that("external packages can substitute every component without changing the workflow", {
  local_adapter_method(
    "dr_read_source",
    "example_source",
    function(source, ...) source$data
  )
  local_adapter_method(
    "dr_check_component",
    "example_source",
    function(x, ...) invisible(x)
  )
  local_adapter_method(
    "dr_execute_transform",
    "example_transform",
    function(transform, data, ...) {
      data$amount <- data$amount * transform$factor
      data
    }
  )
  local_adapter_method(
    "dr_check_component",
    "example_transform",
    function(x, ...) invisible(x)
  )
  written <- NULL
  local_adapter_method(
    "dr_write_target",
    "example_target",
    function(target, data, context, ...) {
      written <<- list(data = data, context = context)
      list(table = context$product, backend = "example")
    }
  )
  local_adapter_method(
    "dr_check_component",
    "example_target",
    function(x, ...) invisible(x)
  )
  data <- data.frame(amount = c(10, 20))
  example_workflow <- function(source, transform, target = NULL) {
    dr_product("orders") |>
      dr_add_source(source) |>
      dr_add_transform(transform) |>
      dr_add_quality(~ amount > 0) |>
      dr_set_target(target)
  }
  native <- example_workflow(data, function(data) {
    transform(data, amount = amount * 2)
  })
  custom <- example_workflow(
    structure(list(data = data), class = "example_source"),
    structure(list(factor = 2), class = "example_transform"),
    structure(list(), class = "example_target")
  )
  a <- dr_run(native)
  b <- dr_run(custom)
  expect_equal(dr_collect(a), dr_collect(b))
  expect_equal(written$data$amount, c(20, 40))
  expect_equal(written$context$metadata$rows, 2L)
  expect_equal(b$outputs$backend, "example")
  written <- NULL
  bad <- custom |> dr_add_quality(~ amount < 0, "impossible")
  expect_equal(dr_run(bad, stop_on_failure = FALSE)$status, "blocked")
  expect_null(written)
})

test_that("unsupported or invalid adapters fail before invoking source callbacks", {
  calls <- 0
  product <- dr_product("orders") |>
    dr_add_source(function() {
      calls <<- calls + 1
      data.frame(id = 1L)
    }) |>
    dr_set_target(structure(list(), class = "unknown_target"))
  expect_snapshot(error = TRUE, dr_validate(product))
  expect_equal(calls, 0)
})
