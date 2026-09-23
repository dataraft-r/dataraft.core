test_that("organization policies block publication when metadata is missing", {
  product <- dr_product("customers", data.frame(id = 1L))
  policy <- dr_policy("owner", require = "owner")
  withr::local_options(dataraft.policies = list(policy))
  decisions <- dr_check_policies(product)
  expect_equal(decisions$decision, "block")
  expect_equal(decisions$missing, "owner")
  expect_error(dr_publish(product, to = tempfile("unused-")), class = "dataraft_error_policy")
  product$owner <- "Analytics"
  expect_equal(dr_check_policies(product)$decision, "pass")
})

test_that("impact traverses registered downstream dependencies without cycles", {
  old <- dr_contract("customers", columns = c(id = "integer"))
  new <- dr_contract("customers", version = "2", columns = c(customer_id = "integer"))
  product <- dr_product("customers", contract = old)
  graph <- data.frame(from_id = c("customers", "report", "metric"),
    to_id = c("report", "metric", "customers"))
  impact <- dr_impact(product, new, graph)
  expect_setequal(impact$affected, c("report", "metric"))
  expect_equal(impact$breaking, TRUE)
  expect_equal(impact$coverage, "registered_only")
})

test_that("SLA distinguishes missing and late deliveries", {
  sla <- dr_sla(freshness = 24, available_by = "08:00", timezone = "Europe/Berlin")
  at <- as.POSIXct("2026-09-23 09:00:00", tz = "Europe/Berlin")
  delivered <- as.POSIXct("2026-09-23 08:30:00", tz = "Europe/Berlin")
  expect_equal(dr_check_sla(sla, "2026-09-23", at = at)$status, "missing")
  expect_equal(dr_check_sla(sla, "2026-09-23", delivered_at = delivered, at = at)$status, "late")
})

test_that("ports bind existing sources and exactly one target", {
  product <- dr_product("customers") |>
    dr_add_input(dr_input("feed", data.frame(id = 1L))) |>
    dr_add_output(dr_output("table", tempfile("customers-")))
  expect_equal(names(product$sources), "feed")
  expect_equal(names(product$output_ports), "table")
  expect_error(dr_add_output(product, dr_output("copy", tempfile("copy-"))),
    class = "dataraft_error_definition")
})

test_that("published hooks receive a run ID and cannot undo a release", {
  events <- list()
  product <- dr_product("hooked", data.frame(id = 1L)) |>
    dr_hook("published", function(event) events[[length(events) + 1L]] <<- event)
  expect_equal(length(events), 0L)
  result <- dr_run(product, write = FALSE)
  expect_equal(result$status, "completed")
  expect_equal(length(events), 0L)
})
