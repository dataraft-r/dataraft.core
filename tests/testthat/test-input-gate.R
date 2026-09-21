test_that("input failures preserve landing and the published release without writing raw", {
  skip_if_not_installed("dataraft.lake")
  f <- fixture()
  withr::defer(fixture_cleanup(f))
  good <- dr_run(f$pipeline, f$lake)
  contract <- f$contract
  contract$rules <- list(dr_quality_rule("nonnegative", function(data) {
    all(data$reserve >= 0)
  }))
  contract$id <- "risk.input"
  p <- dr_pipeline("risk.prechecked", f$lake, code_version = "v1") |>
    dr_step_land(f$pipeline$steps$land) |>
    dr_step_extract() |>
    dr_step_precheck(contract) |>
    dr_step_validate(f$contract) |>
    dr_step_publish("risk.validated")
  bad <- f$good
  bad$reserve[1] <- -10
  f$write(bad)
  notifications <- list()
  result <- dr_run(
    p,
    f$lake,
    stop_on_failure = FALSE,
    notify = function(event) {
      notifications[[length(notifications) + 1L]] <<- event
    }
  )
  expect_equal(result$status, "blocked")
  expect_equal(
    unique(dr_quality(f$lake, run_id = result$run_id)$stage),
    "ingest"
  )
  expect_equal(
    dr_releases(f$lake, "risk.validated")$release_id,
    good$release_id
  )
  expect_equal(
    DBI::dbExistsTable(
      f$lake$con,
      DBI::Id(
        catalog = "lake",
        schema = "raw",
        table = paste0("raw_", result$run_id)
      )
    ),
    FALSE
  )
  inputs <- dr_registry(f$lake, "inputs")
  expect_equal(
    file.exists(inputs$landed_path[inputs$run_id == result$run_id]),
    TRUE
  )
  expect_equal(notifications[[1]]$type, "quality_failed")
  f$write()
  success <- dr_run(p, f$lake)
  expect_equal(success$status, "published")
  expect_setequal(dr_quality(success)$stage, c("ingest", "candidate"))
  expect_equal(
    dr_plan(p)$step,
    c("land", "extract", "precheck", "validate", "publish")
  )
})

test_that("the final candidate gate still runs after a successful input gate", {
  skip_if_not_installed("dataraft.lake")
  f <- fixture()
  withr::defer(fixture_cleanup(f))
  input <- f$contract
  input$id <- "risk.input"
  input$rules <- list()
  p <- dr_pipeline("risk.prechecked", f$lake, code_version = "v1") |>
    dr_step_land(f$pipeline$steps$land) |>
    dr_step_extract() |>
    dr_step_precheck(input) |>
    pipeline_step_transform(
      function(data) dplyr::mutate(data, reserve = -reserve),
      "negate"
    ) |>
    dr_step_validate(f$contract) |>
    dr_step_publish("risk.validated")
  result <- dr_run(p, f$lake, stop_on_failure = FALSE)
  expect_equal(result$status, "blocked")
  expect_equal(
    all(result$quality$status[result$quality$stage == "ingest"] == "passed"),
    TRUE
  )
  expect_equal(
    any(result$quality$status[result$quality$stage == "candidate"] == "failed"),
    TRUE
  )
})
