test_that("reports escape metadata, expose the gate and never include retained agents", {
  contract <- dr_contract(
    "orders",
    "1",
    "Analytics",
    "Orders",
    "One order",
    c(id = "integer"),
    key = "id",
    rules = list(dr_quality_rule("<script>alert(1)</script>", function(data) {
      FALSE
    }))
  )
  quality <- dr_validate(data.frame(id = 1L), contract)
  root <- withr::local_tempdir()
  html <- file.path(root, "quality.html")
  json <- file.path(root, "quality.json")
  dr_quality_report(quality, html)
  text <- paste(readLines(html), collapse = "\n")
  expect_match(text, "Publication gate: Blocked", fixed = TRUE)
  expect_match(text, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_equal(grepl("<script>", text, fixed = TRUE), FALSE)
  dr_quality_report(quality, json, format = "json")
  evidence <- jsonlite::read_json(json)
  expect_equal(evidence$publication_allowed, FALSE)
  expect_equal(length(evidence$checks), nrow(quality))
  expect_snapshot(error = TRUE, dr_quality_report(quality, html))
})

test_that("native pointblank HTML can be exported from retained agents", {
  skip_if_not_installed("pointblank")
  contract <- dr_contract(
    "amounts",
    "1",
    "Analytics",
    "Amounts",
    "One amount",
    c(amount = "numeric"),
    rules = list(dr_pointblank_checks("positive", function(data) {
      pointblank::create_agent(data) |> pointblank::col_vals_gte("amount", 0)
    }))
  )
  quality <- dr_validate(
    data.frame(amount = c(10, -1)),
    contract,
    keep_agents = TRUE
  )
  path <- withr::local_tempfile(fileext = ".html")
  dr_pointblank_report(quality, "positive", path)
  expect_gt(file.info(path)$size, 1000)
  agent <- attr(quality, "pointblank_agents")$positive
  expect_equal(length(agent$extracts), 0L)
  expect_equal(all(is.na(agent$validation_set$row_sample)), TRUE)
})
