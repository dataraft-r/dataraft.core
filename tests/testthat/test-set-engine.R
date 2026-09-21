test_that("quality engines change implementation without changing the predicate", {
  spec <- dr_quality_rule("positive", ~ amount > 0)
  pointblank <- dr_set_engine(spec, "pointblank")
  native <- dr_set_engine(pointblank, "native")
  expect_identical(pointblank$check, spec$check)
  expect_identical(pointblank$engine, "pointblank")
  expect_identical(spec$engine, "r")
  expect_equal(
    dr_run_quality(native, data.frame(amount = c(1, -1)))$n_failed,
    1
  )
})

test_that("reusable lookup specs retain dependency resolution", {
  lookup <- dr_lookup_spec(
    data.frame(id = 1:2, label = c("a", "b")),
    by = "id",
    name = "labels"
  )
  alternate <- dr_set_engine(lookup, "dm")
  expect_identical(alternate$by, lookup$by)
  expect_identical(lookup$engine, "native")
  flow <- dr_workflow() |>
    dr_add_product(dr_product("orders")) |>
    dr_add_recipe(dr_recipe() |> dr_step_transform(lookup))
  expect_equal(
    dr_collect(dr_trial(flow, data = data.frame(id = 1:2)))$label,
    c("a", "b")
  )
})

test_that("engine selection rejects unsupported options", {
  expect_snapshot(
    error = TRUE,
    dr_quality_rule("positive", ~ amount > 0) |> dr_set_engine("spark")
  )
})
test_that("function checks cannot silently become pointblank builders", {
  expect_snapshot(
    error = TRUE,
    dr_quality_rule("positive", function(data) all(data$amount > 0)) |>
      dr_set_engine("pointblank")
  )
})

test_that("supported quality engines agree on failure counts", {
  skip_if_not_installed("pointblank")
  spec <- dr_quality_rule("positive", ~ amount > 0)
  data <- data.frame(amount = c(1, -1, NA))
  native <- dr_run_quality(dr_set_engine(spec, "native"), data)
  alternative <- dr_run_quality(dr_set_engine(spec, "pointblank"), data)
  expect_equal(alternative$n_failed, native$n_failed)
  expect_equal(alternative$status, native$status)
})

test_that("specification printing shows engines without reading references", {
  lookup <- dr_lookup_spec(
    function() stop("must not read"),
    by = "id",
    name = "reference"
  )
  expect_output(print(lookup), "Engine: native")
  expect_output(
    print(dr_quality_rule("positive", ~ amount > 0)),
    "Engine: native"
  )
})
