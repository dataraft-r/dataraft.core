test_that("public evidence preserves only dataset identity fields", {
  result <- safe_descriptor(list(
    dataset = list(
      namespace = "postgres://warehouse:5432",
      name = "insurance.public.orders",
      password = "never-public"
    )
  ))
  expect_identical(
    result$dataset,
    list(
      namespace = "postgres://warehouse:5432",
      name = "insurance.public.orders"
    )
  )
})
