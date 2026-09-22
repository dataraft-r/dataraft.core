test_that("a third-party backend can define layers and pinned outputs", {
  registry <- get(".__S3MethodsTable__.", envir = asNamespace("dataraft.core"))
  methods <- list(
    dr_set_target_layer = function(x, layer, ...) { x$layer <- layer; x },
    dr_read_output = function(x, asset, release, ...) {
      data.frame(asset = asset, release = release)
    },
    dr_output_source = function(x, asset, release, ...) {
      structure(list(asset = asset, release = release), class = "third_party_source")
    }
  )
  entries <- paste(names(methods), "third_party_backend", sep = ".")
  withr::defer(rm(list = entries, envir = registry))
  for (generic in names(methods)) {
    registerS3method(generic, "third_party_backend", methods[[generic]],
      envir = asNamespace("dataraft.core"))
  }
  backend <- structure(list(), class = "third_party_backend")
  expect_identical(dr_set_target_layer(backend, "curated")$layer, "curated")
  expect_identical(dr_read_output(backend, "orders", "rel_1"),
    data.frame(asset = "orders", release = "rel_1"))
  expect_s3_class(dr_output_source(backend, "orders", "rel_1"), "third_party_source")
  expect_identical(backend, structure(list(), class = "third_party_backend"))
})

test_that("unsupported backend operations fail before writing", {
  expect_error(dr_read_output(list(), "orders", "rel_1"), class = "dataraft_error_definition")
  expect_error(dr_set_target_layer(list(), "curated"), class = "dataraft_error_definition")
  expect_error(dr_acquire_write_session(list()), class = "dataraft_error_definition")
})

test_that("read context does not leak into existing source adapters", {
  reader <- function() data.frame(value = 1)
  expect_identical(dr_read_input(reader, context = list(connection = "unused")),
    data.frame(value = 1))
})
