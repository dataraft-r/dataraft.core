test_that("workflow slots can be reused, updated, removed and bound at execution", {
  spec <- dr_product("orders") |> dr_add_quality(~ amount >= 0)
  preparation <- dr_recipe() |> dr_step_mutate(amount = amount * 2)
  flow <- dr_workflow() |> dr_add_product(spec) |> dr_add_recipe(preparation)
  original <- flow
  expect_identical(dr_extract_product(flow), spec)
  expect_identical(dr_extract_recipe(flow), preparation)
  first <- dr_trial(flow, data = data.frame(amount = 10))
  second <- dr_trial(flow, data = data.frame(amount = 30))
  expect_equal(dr_collect(first)$amount, 20)
  expect_equal(dr_collect(second)$amount, 60)
  changed <- dr_update_recipe(
    flow,
    dr_recipe() |> dr_step_mutate(amount = amount + 1)
  )
  expect_equal(
    dr_collect(dr_trial(changed, data = data.frame(amount = 10)))$amount,
    11
  )
  expect_equal(
    dr_collect(dr_trial(
      dr_remove_recipe(flow),
      data = data.frame(amount = 10)
    ))$amount,
    10
  )
  renamed <- dr_update_product(flow, dr_product("new_orders"))
  expect_equal(
    dr_run(renamed, data = data.frame(amount = 10))$asset,
    "new_orders"
  )
  expect_null(dr_remove_product(flow)$product)
  expect_identical(flow, original)
})

test_that("preflight and printing never read sources", {
  calls <- 0L
  flow <- dr_workflow() |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = amount * 2)) |>
    dr_add_source(function() {
      calls <<- calls + 1L
      data.frame(amount = 10)
    }) |>
    dr_add_product(dr_product("orders"))
  expect_identical(dr_validate(flow), flow)
  expect_identical(attr(dr_plan(flow), "complete"), TRUE)
  expect_output(print(flow), "orders")
  expect_equal(dr_inspect(flow)$type, "product workflow")
  expect_identical(calls, 0L)
  expect_equal(dr_collect(dr_run(flow))$amount, 20)
  expect_identical(calls, 1L)
})

test_that("failed gates and trials never invoke the configured writer", {
  writes <- 0L
  local_adapter_method(
    "dr_check_component",
    "workflow_test_target",
    function(x, ...) invisible(x)
  )
  local_adapter_method(
    "dr_write_target",
    "workflow_test_target",
    function(target, data, context, ...) {
      writes <<- writes + 1L
      list(type = "test", rows = nrow(data))
    }
  )
  flow <- dr_workflow() |>
    dr_add_product(dr_product("orders") |> dr_add_quality(~ amount > 0)) |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = amount * 2)) |>
    dr_set_target(structure(list(), class = "workflow_test_target"))
  good <- data.frame(amount = 10)
  bad <- data.frame(amount = -10)
  expect_equal(dr_trial(flow, data = good)$status, "completed")
  expect_identical(writes, 0L)
  expect_equal(
    dr_publish(flow, data = bad, stop_on_failure = FALSE)$status,
    "blocked"
  )
  expect_identical(writes, 0L)
  expect_equal(dr_collect(dr_publish(flow, data = good))$amount, 20)
  expect_identical(writes, 1L)
})

test_that("bound deliveries can change without mutating sources or recipe", {
  flow <- dr_workflow() |>
    dr_add_product(dr_product("orders")) |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = amount * 2)) |>
    dr_add_source(data.frame(amount = 10), name = "delivery")
  expect_equal(
    dr_collect(dr_trial(flow, data = data.frame(amount = 20)))$amount,
    40
  )
  expect_equal(
    dr_collect(dr_trial(
      flow,
      sources = list(delivery = data.frame(amount = 30))
    ))$amount,
    60
  )
  expect_equal(dr_collect(dr_trial(flow))$amount, 20)
})

test_that("workflow execution defaults and explicit rule engines remain separate", {
  flow <- dr_workflow(
    execution = dr_execution_config(quality = "pointblank")
  ) |>
    dr_add_product(
      dr_product("orders") |>
        dr_add_quality(
          dr_quality_rule("positive", ~ amount > 0) |> dr_set_engine("native")
        )
    )
  expect_equal(
    dr_trial(flow, data = data.frame(amount = 10))$status,
    "completed"
  )
})

test_that("invalid or conflicting slots explain how to repair them", {
  expect_snapshot(error = TRUE, dr_add_product(dr_workflow(), 1))
})
test_that("adding an occupied product slot requires update", {
  expect_snapshot(
    error = TRUE,
    dr_workflow() |>
      dr_add_product(dr_product("a")) |>
      dr_add_product(dr_product("b"))
  )
})
test_that("adding an occupied recipe slot requires update", {
  expect_snapshot(
    error = TRUE,
    dr_workflow() |> dr_add_recipe(dr_recipe()) |> dr_add_recipe(dr_recipe())
  )
})
test_that("preparation stays in a dedicated workflow slot", {
  expect_snapshot(
    error = TRUE,
    dr_workflow() |>
      dr_add_product(dr_product("a") |> dplyr::mutate(amount = 1))
  )
})
test_that("sources cannot silently shadow product inputs", {
  expect_snapshot(
    error = TRUE,
    dr_workflow() |>
      dr_add_source(data.frame(id = 1L)) |>
      dr_add_product(dr_product("a", data.frame(id = 2L)))
  )
})
test_that("incomplete workflows explain the missing product", {
  expect_snapshot(
    error = TRUE,
    dr_trial(dr_workflow(), data = data.frame(id = 1L))
  )
})

test_that("model products use the same assembly without flattening", {
  skip_if_not_installed("dm")
  model <- dm::dm(
    customers = data.frame(id = 1:2),
    policies = data.frame(policy_id = 1:2, customer_id = 1:2)
  ) |>
    dm::dm_add_pk(customers, id) |>
    dm::dm_add_pk(policies, policy_id) |>
    dm::dm_add_fk(policies, customer_id, customers)
  flow <- dr_workflow() |> dr_add_product(dr_product("portfolio", model))
  result <- dr_trial(flow)
  expect_s3_class(dr_collect(result), "dm")
  expect_equal(nrow(dr_collect(result)$policies), 2L)
})

test_that("nested modular workflows share dependencies and trials disable their writers", {
  reads <- 0L
  writes <- 0L
  local_adapter_method(
    "dr_check_component",
    "nested_workflow_target",
    function(x, ...) invisible(x)
  )
  local_adapter_method(
    "dr_write_target",
    "nested_workflow_target",
    function(target, data, context, ...) {
      writes <<- writes + 1L
      list(type = "test")
    }
  )
  upstream <- dr_workflow() |>
    dr_add_product(dr_product("upstream")) |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = amount * 2)) |>
    dr_add_source(function() {
      reads <<- reads + 1L
      data.frame(id = 1:2, amount = c(10, 20))
    }) |>
    dr_set_target(structure(list(), class = "nested_workflow_target"))
  downstream <- dr_workflow() |>
    dr_add_product(dr_product("downstream")) |>
    dr_add_source(upstream) |>
    dr_add_recipe(
      dr_recipe() |> dr_step_lookup(upstream, by = "id", name = "same")
    )
  result <- dr_trial(downstream)
  expect_equal(dr_collect(result)$amount.x, c(20, 40))
  expect_identical(reads, 1L)
  expect_identical(writes, 0L)
})

test_that("lake workflow publications retain checked immutable releases", {
  skip_if_not_installed("duckdb")
  destination <- withr::local_tempdir()
  flow <- dr_workflow() |>
    dr_add_product(dr_product("orders") |> dr_add_quality(~ amount >= 0)) |>
    dr_add_recipe(dr_recipe() |> dr_step_mutate(amount = amount * 2))
  first <- dr_publish(flow, to = destination, data = data.frame(amount = 10))
  second <- dr_publish(flow, to = destination, data = data.frame(amount = 20))
  expect_equal(dr_collect(first)$amount, 20)
  expect_equal(dr_collect(second)$amount, 40)
  expect_equal(
    dr_publish(
      flow,
      to = destination,
      data = data.frame(amount = -1),
      stop_on_failure = FALSE
    )$status,
    "blocked"
  )
  expect_equal(dr_collect(second)$amount, 40)
})
