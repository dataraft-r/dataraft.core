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

test_that("attached policies apply without session options and are retained in evidence", {
  skip_if_not_installed("dataraft.adapters")
  product <- dr_product("customers", data.frame(id = 1L)) |>
    dr_add_policy(dr_policy("owner", require = "owner", version = "2"))
  expect_equal(dr_check_policies(product)$decision, "block")
  expect_error(dr_publish(product, to = tempfile("blocked-")), class = "dataraft_error_policy")
  product$owner <- "Analytics"
  evidence <- withr::local_tempdir()
  result <- dr_run(dr_set_target(product, dataraft.adapters::dr_target_rds(
    withr::local_tempfile())), evidence = evidence)
  expect_equal(result$status, "published")
  record <- dr_read_run(evidence, result$run_id)
  expect_equal(record$policies[[1]]$id, "owner")
  expect_equal(record$policies[[1]]$version, "2")
  expect_equal(record$policies[[1]]$decision, "pass")
})

test_that("validate policies are checked during configuration validation", {
  product <- dr_product("orders", data.frame(id = 1L)) |>
    dr_add_policy(dr_policy("owner", when = "validate", require = "owner"))
  expect_error(dr_validate(product), class = "dataraft_error_policy")
  product$owner <- "Analytics"
  expect_s3_class(dr_validate(product), "dr_product")
  expect_error(dr_add_policy(product, dr_policy("owner", require = "description")),
    class = "dataraft_error_definition")
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

test_that("an output SLA is evaluated on successful publication and retained as evidence", {
  skip_if_not_installed("dataraft.adapters")
  path <- withr::local_tempfile(fileext = ".rds")
  evidence <- withr::local_tempdir()
  product <- dr_product("sla.delivery", data.frame(id = 1L)) |>
    dr_add_output(dr_output("table", dataraft.adapters::dr_target_rds(path),
      sla = dr_sla(available_by = "08:00", timezone = "UTC")))
  expect_error(dr_run(product), "business_date")
  expect_false(file.exists(path))
  result <- dr_run(product, business_date = "2026-09-23", evidence = evidence)
  expect_equal(result$status, "published")
  expect_equal(result$metadata$sla$table$status, "late")
  expect_equal(dr_read_run(evidence, result$run_id)$sla$table$status, "late")
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

test_that("multiple output ports share one checked delivery", {
  skip_if_not_installed("dataraft.adapters")
  first <- withr::local_tempfile()
  second <- withr::local_tempfile()
  evidence <- withr::local_tempdir()
  reads <- 0L
  product <- dr_product("multiple.ports") |>
    dr_add_source(function() { reads <<- reads + 1L; data.frame(id = 1L) }) |>
    dr_add_output(dr_output("primary", dataraft.adapters::dr_target_rds(first))) |>
    dr_add_output(dr_output("secondary", dataraft.adapters::dr_target_rds(second)))
  expect_error(dr_run(product, cache = TRUE), "cache = FALSE")
  expect_equal(reads, 0L)
  result <- dr_run(product, evidence = evidence)
  expect_equal(reads, 1L)
  expect_equal(result$status, "published")
  expect_equal(vapply(result$port_outputs, `[[`, "", "status"),
    c(primary = "published", secondary = "published"))
  expect_true(dir.exists(first))
  expect_true(dir.exists(second))
  saved <- dr_read_run(evidence, result$run_id)
  expect_equal(saved$port_outputs$primary$status, "published")
  expect_equal(saved$port_outputs$secondary$status, "published")
})

test_that("a failed secondary output reports committed primary output", {
  skip_if_not_installed("dataraft.adapters")
  first <- withr::local_tempfile()
  blocked <- withr::local_tempfile()
  evidence <- withr::local_tempdir()
  writeLines("not a directory", blocked)
  product <- dr_product("partial.ports", data.frame(id = 1L)) |>
    dr_add_output(dr_output("primary", dataraft.adapters::dr_target_rds(first))) |>
    dr_add_output(dr_output("secondary", dataraft.adapters::dr_target_rds(blocked)))
  result <- dr_run(product, stop_on_failure = FALSE, evidence = evidence)
  expect_equal(result$status, "error")
  expect_equal(result$port_outputs$primary$status, "published")
  expect_equal(result$port_outputs$secondary$status, "failed")
  expect_true(dir.exists(first))
  expect_equal(dr_read_run(evidence, result$run_id)$port_outputs$secondary$status,
    "failed")
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

test_that("retry resumes after a committed primary without writing it twice", {
  skip_if_not_installed("dataraft.adapters")
  first <- withr::local_tempfile()
  blocked <- withr::local_tempfile()
  evidence <- withr::local_tempdir()
  writeLines("not a directory", blocked)
  product <- dr_product("retry.ports", data.frame(id = 1L)) |>
    dr_add_output(dr_output("primary", dataraft.adapters::dr_target_rds(first))) |>
    dr_add_output(dr_output("secondary", dataraft.adapters::dr_target_rds(blocked)))
  result <- dr_run(product, stop_on_failure = FALSE, evidence = evidence)
  expect_equal(result$status, "error")
  before <- list.files(first, recursive = TRUE)
  expect_error(dr_retry_ports(product, dr_read_run(evidence, result$run_id)),
    class = "dataraft_error_definition")
  unlink(blocked)
  resumed <- dr_retry_ports(product, result, evidence = evidence)
  expect_equal(resumed$status, "published")
  expect_equal(resumed$run_id, result$run_id)
  expect_equal(vapply(resumed$port_outputs, `[[`, "", "status"),
    c(primary = "published", secondary = "published"))
  expect_equal(list.files(first, recursive = TRUE), before)
  expect_equal(dr_read_run(evidence, result$run_id)$port_outputs$secondary$status,
    "published")
  expect_error(dr_retry_ports(product, resumed), class = "dataraft_error_definition")
})
