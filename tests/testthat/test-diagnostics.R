test_that("quality accessors distinguish latest failure, pinned release and cache", {
  f <- fixture()
  withr::defer(fixture_cleanup(f))
  first <- dr_run(f$pipeline, f$lake)
  cached <- dr_run(f$pipeline, f$lake)
  expect_equal(
    dr_quality(f$lake, run_id = cached$run_id)$run_id,
    rep(first$run_id, nrow(first$quality))
  )
  bad <- f$good
  bad$reserve[1] <- -1
  f$write(bad)
  dr_run(f$pipeline, f$lake, stop_on_failure = FALSE)
  expect_equal(
    any(dr_quality(f$lake, asset = "risk.validated")$status == "failed"),
    TRUE
  )
  dr_expect_quality(dr_quality(
    f$lake,
    asset = "risk.validated",
    release = first$release_id
  ))
  expect_equal(dr_status(f$lake, "risk.validated")$status[[1]], "blocked")
  expect_equal(dr_status(first)$success, TRUE)
})

test_that("process errors stay visible beside successful dbt nodes", {
  skip_if_not_installed("dataraft.dbt")
  parsed <- dataraft.dbt:::dbt_read_artifacts(system.file(
    "extdata",
    "dbt-artifacts",
    package = "dataraft.dbt"
  ))
  result <- structure(
    list(
      success = FALSE,
      status = 2L,
      command = "build",
      results = parsed$results,
      manifest = parsed$manifest
    ),
    class = "dr_dbt_result"
  )
  expect_equal(tail(dr_status(result)$id, 1), ".process")
  expect_equal(dataraft.core:::quality_ok(dr_quality(result)), FALSE)
  edges <- dr_lineage(result, "seed.shop.raw_orders", direction = "downstream")
  expect_equal(nrow(edges), 2L)
  expect_equal(
    nrow(dr_lineage(
      result,
      "seed.shop.raw_orders",
      direction = "downstream",
      recursive = FALSE
    )),
    1L
  )
})
