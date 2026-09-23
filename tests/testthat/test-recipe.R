test_that("recipes reuse deferred tidy expressions without changing definitions", {
  calls <- 0L
  threshold <- 10
  preparation <- dr_recipe() |>
    dr_step_mutate(amount = amount * 2) |>
    dr_step_filter(amount > !!threshold) |>
    dr_step_select(id, amount) |>
    dr_step_arrange(id)
  original <- preparation
  source <- function() {
    calls <<- calls + 1L
    data.frame(id = c(3L, 2L, 1L), amount = c(5, 20, 10), ignored = 0)
  }
  a <- dr_product("a", source) |> dr_add_recipe(preparation)
  b <- dr_product("b", data.frame(id = 4L, amount = 30)) |>
    dr_add_recipe(preparation)
  expect_identical(calls, 0L)
  expect_equal(
    dr_collect(dr_run(write = FALSE, stop_on_failure = FALSE, a)),
    tibble::tibble(id = 1:2, amount = c(20, 40))
  )
  expect_equal(
    dr_collect(dr_run(write = FALSE, stop_on_failure = FALSE, b))$amount,
    60
  )
  expect_identical(calls, 1L)
  expect_identical(preparation, original)
})

test_that("summaries, selection and custom transformations retain ordinary semantics", {
  preparation <- dr_recipe() |>
    dr_step_distinct(group, amount, .keep_all = TRUE) |>
    dr_step_rename(value = amount) |>
    dr_step_summarise(total = sum(value), .by = group) |>
    dr_step_transform(~ dplyr::mutate(.x, total = total + 1))
  data <- data.frame(group = c("a", "a", "a", "b"), amount = c(1, 1, 2, 4))
  out <- dr_product("summary", data) |>
    dr_add_recipe(preparation) |>
    dr_run(write = FALSE, stop_on_failure = FALSE) |>
    dr_collect()
  expect_equal(out, tibble::tibble(group = c("a", "b"), total = c(4, 5)))
})

test_that("recipe lookups remain replaceable shared execution dependencies", {
  calls <- 0L
  customers <- dr_product("customers", function() {
    calls <<- calls + 1L
    data.frame(id = 1:2, region = c("North", "South"))
  })
  preparation <- dr_recipe() |> dr_step_lookup(customers, by = "id")
  flow <- dr_workflow() |>
    dr_add_product(dr_product("orders")) |>
    dr_add_recipe(preparation) |>
    dr_add_source(data.frame(id = 1:2), name = "orders")
  expect_identical(calls, 0L)
  expect_equal(
    dr_collect(dr_run(write = FALSE, stop_on_failure = FALSE, flow))$region,
    c("North", "South")
  )
  expect_identical(calls, 1L)
  corrected <- dr_run(
    write = FALSE,
    stop_on_failure = FALSE,
    flow,
    sources = list(customers = data.frame(id = 1:2, region = c("East", "West")))
  )
  expect_equal(dr_collect(corrected)$region, c("East", "West"))
  expect_identical(calls, 1L)
})
