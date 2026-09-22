test_that("third-party diagnostics work without sibling package dispatch", {
  provider <- structure(list(), class = "review_metadata_provider")
  methods <- list(
    dr_status = function(x, asset = NULL, ...) data.frame(status = "ready"),
    dr_quality = function(x, run_id = NULL, asset = NULL, release = NULL, ...) {
      data.frame(rule = "external", status = "passed", n_failed = 0, n_total = 2)
    },
    dr_lineage_edges = function(x, ...) data.frame(
      run_id = c("r1", "r2", "r3"),
      from_id = c("a", "b", "c"), from_version = "1",
      to_id = c("b", "c", "a"), to_version = "1", relation = "input"
    ),
    dr_metadata_rows = function(lake, table, asset = NULL, run_id = NULL, ...) {
      data.frame(table = table, asset = asset, run_id = run_id)
    }
  )
  registry <- get(".__S3MethodsTable__.", envir = asNamespace("dataraft.core"))
  names <- paste(names(methods), "review_metadata_provider", sep = ".")
  withr::defer(rm(list = names, envir = registry))
  for (generic in names(methods)) {
    registerS3method(generic, "review_metadata_provider", methods[[generic]],
      envir = asNamespace("dataraft.core"))
  }
  expect_identical(dr_status(provider)$status, "ready")
  expect_identical(dr_quality(provider)$rule, "external")
  expect_equal(nrow(dr_lineage(provider, "a")), 3L)
  expect_identical(dr_lineage(provider, "a", recursive = FALSE)$from_id, "c")
  expect_identical(dr_metadata_rows(provider, "runs", "orders", "r1"),
    data.frame(table = "runs", asset = "orders", run_id = "r1"))
})

test_that("plain quality evidence retains normalized failure rates", {
  evidence <- data.frame(rule = "key", status = "failed", n_failed = 1, n_total = 4)
  expect_equal(dr_quality(evidence)$failure_rate, 0.25)
})

test_that("legacy measurement manifests are promoted without changing values", {
  values <- data.frame(total = 4)
  attr(values, "dr_manifest") <- list(metric = "total")
  promoted <- diagnostic_dispatch_object(values)
  expect_s3_class(promoted, "dr_measurement")
  expect_identical(promoted$total, values$total)
  expect_identical(attr(promoted, "dr_manifest"), attr(values, "dr_manifest"))
  expect_identical(class(values), "data.frame")
})
