test_that("absolute paths stay canonical when missing directories are created", {
  root <- withr::local_tempdir()
  path <- file.path(root, ".", "new directory", "evidence")
  expected <- file.path(
    normalizePath(root, winslash = "/", mustWork = TRUE),
    "new directory",
    "evidence"
  )
  before <- absolute_path(path)
  expect_identical(before, expected)
  expect_equal(dir.exists(dirname(path)), FALSE)
  dir.create(path, recursive = TRUE)
  expect_identical(absolute_path(path), before)
  expect_identical(before, normalizePath(path, winslash = "/", mustWork = TRUE))
})
