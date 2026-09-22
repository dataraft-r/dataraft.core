test_that("execution defaults are explicit values and definitions stay unchanged", {
  reads <- 0L
  definition <- dr_product("orders", function() {
    reads <<- reads + 1L
    data.frame(id = 1:2)
  }) |>
    dr_add_quality(~ id > 0)
  original <- definition
  execution <- dr_execution_config()
  expect_equal(reads, 0L)
  result <- dr_run(definition, execution = execution)
  expect_equal(dr_collect(result)$id, 1:2)
  expect_equal(reads, 1L)
  expect_identical(definition, original)
  expect_identical(execution, dr_execution_config())
})

test_that("formula engines propagate through contracts and lookup dependencies", {
  skip_if_not_installed("pointblank")
  skip_if_not_installed("dm")
  reference <- dr_product(
    "customers",
    data.frame(id = 1:2, region = c("a", "b"))
  ) |>
    dr_add_contract(dr_contract(columns = c(id = "integer", region = "character"))) |>
    dr_add_quality(~ id > 0, name = "customer_positive")
  definition <- dr_product("orders", data.frame(id = 1:2)) |>
    dr_add_contract(dr_contract(
      "orders_schema",
      columns = c(id = "integer", region = "character"),
      rules = list(dr_quality_rule("contract_positive", ~ id > 0))
    )) |>
    dr_add_quality(~ id > 0, name = "inherited") |>
    dr_add_quality(~ id > 0, name = "explicit", engine = "native") |>
    dr_add_quality(function(data) data$id > 0, name = "function") |>
    dr_add_lookup(reference, by = "id")
  execution <- dr_execution_config(quality = "pointblank", relationships = "dm")
  result <- dr_run(definition, execution = execution)
  rules <- dr_quality(result)
  expect_equal(
    rules$engine[match(
      c("contract_positive", "inherited", "explicit", "function"),
      rules$rule
    )],
    c("pointblank", "pointblank", "r", "r")
  )
  expect_equal(dr_collect(result)$region, c("a", "b"))
  resolved <- apply_execution_defaults(definition, execution)
  expect_identical(resolved$transforms[[1]]$engine, "dm")
  expect_identical(
    resolved$transforms[[1]]$source$quality[[1]]$engine,
    "pointblank"
  )
  explicit <- definition |>
    dr_add_lookup(data.frame(id = 1:2), by = "id", engine = "native")
  expect_identical(
    apply_execution_defaults(explicit, execution)$transforms[[2]]$engine,
    "native"
  )
})

test_that("defaults preserve configured dependency targets and layers", {
  skip_if_not_installed("dataraft.lake")
  root <- withr::local_tempdir()
  dependency <- dr_product("customers", data.frame(id = 1L)) |>
    dr_set_target(dr_target_lake(
      file.path(root, "existing"),
      layer = "validated"
    ))
  definition <- dr_product("orders", dependency)
  execution <- dr_execution_config(
    to = file.path(root, "default"),
    layer = "staging"
  )
  resolved <- apply_execution_defaults(definition, execution)
  expect_identical(resolved$sources[[1]]$target, dependency$target)
  expect_identical(resolved$target$layer, "staging")
  expect_identical(dir.exists(file.path(root, "default")), FALSE)
})

test_that("publication gates still prevent writes with alternate defaults", {
  skip_if_not_installed("pointblank")
  writes <- 0L
  local_family_bindings(dr_write_target.NULL = function(...) {
    writes <<- writes + 1L
    list(type = "memory")
  })
  result <- dr_product("orders", data.frame(id = -1L)) |>
    dr_add_quality(~ id > 0) |>
    dr_run(
      execution = dr_execution_config(quality = "pointblank"),
      stop_on_failure = FALSE
    )
  expect_identical(result$status, "blocked")
  expect_equal(writes, 0L)
})

test_that("cycles are rejected before source acquisition", {
  reads <- 0L
  child <- dr_product("orders", function() {
    reads <<- reads + 1L
    data.frame(id = 1L)
  })
  parent <- dr_product("orders", child)
  condition <- tryCatch(
    dr_run(parent, execution = dr_execution_config()),
    error = identity
  )
  expect_s3_class(condition, "dr_dependency_cycle")
  expect_equal(reads, 0L)
})

test_that("invalid defaults are rejected clearly", {
  expect_snapshot(error = TRUE, dr_execution_config(quality = "unknown"))
})

test_that("incompatible layers are rejected before source acquisition", {
  skip_if_not_installed("dataraft.lake")
  reads <- 0L
  source <- function() {
    reads <<- reads + 1L
    data.frame(id = 1L)
  }
  condition <- tryCatch(
    dr_run(
      dr_product("orders", source),
      execution = dr_execution_config(layer = "staging")
    ),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_match(conditionMessage(condition), "requires a lake target")
  condition <- tryCatch(
    dr_ingest(source, execution = dr_execution_config(layer = "staging")),
    error = identity
  )
  expect_s3_class(condition, "error")
  expect_match(
    conditionMessage(condition),
    "Ingestion requires execution layer"
  )
  expect_equal(reads, 0L)
})

test_that("explicit publication destination and layer override root defaults", {
  skip_if_not_installed("dataraft.lake")
  captured <- NULL
  local_family_bindings(dr_run = function(pipeline, execution, ...) {
    captured <<- apply_execution_defaults(pipeline, execution)
    captured
  })
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  definition <- dr_product("orders", data.frame(id = 1L))
  execution <- dr_execution_config(
    to = file.path(root, "defaults"),
    layer = "staging"
  )
  dr_publish(
    definition,
    to = file.path(root, "explicit"),
    layer = "validated",
    execution = execution
  )
  expect_identical(captured$target$destination, file.path(root, "explicit"))
  expect_identical(captured$target$layer, "validated")
  dr_publish(definition, execution = execution)
  expect_identical(captured$target$destination, file.path(root, "defaults"))
  expect_identical(captured$target$layer, "staging")
})

test_that("ingestion resolves default destinations and engines before connecting", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("pointblank")
  root <- withr::local_tempdir()
  seen <- NULL
  local_family_bindings(with_execution_lake = function(to, fun) {
    seen <<- list(to = to, definition = environment(fun)$definition)
    invisible(NULL)
  })
  defaults <- dr_execution_config(
    quality = "pointblank",
    to = file.path(root, "default")
  )
  dr_ingest(data.frame(id = 1L), quality = ~ id > 0, execution = defaults)
  expect_identical(seen$to, dr_lake_config(path = file.path(root, "default")))
  expect_identical(seen$definition$quality[[1]]$engine, "pointblank")
  dr_ingest(
    data.frame(id = 1L),
    to = file.path(root, "explicit"),
    execution = defaults
  )
  expect_identical(seen$to, dr_lake_config(path = file.path(root, "explicit")))
  expect_identical(dir.exists(file.path(root, "default")), FALSE)
})

test_that("dbt rejects product execution defaults before invoking its runner", {
  skip_if_not_installed("dataraft.dbt")
  project <- structure(list(), class = "dr_dbt_project")
  result <- structure(list(), class = "dr_dbt_result")
  condition <- tryCatch(
    dr_run(project, execution = dr_execution_config()),
    error = identity
  )
  expect_match(conditionMessage(condition), "Configure dbt through its project")
  condition <- tryCatch(
    dr_publish(result, execution = dr_execution_config()),
    error = identity
  )
  expect_match(conditionMessage(condition), "Configure dbt through its project")
})


test_that("execution destinations do not publish intermediate dependencies", {
  skip_if_not_installed("dataraft.lake")
  root <- withr::local_tempdir()
  child <- dr_product("customers", data.frame(id = 1L))
  definition <- dr_product("orders", child)
  resolved <- apply_execution_defaults(
    definition,
    dr_execution_config(to = root, layer = "staging")
  )
  expect_null(resolved$sources[[1]]$target)
  expect_identical(resolved$target$layer, "staging")
})

test_that("engine defaults preserve declared contracts and immutable releases", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("pointblank")
  lake <- dr_open_lake(withr::local_tempdir())
  withr::defer(dr_close_lake(lake))
  declaration <- dr_contract(
    "orders_schema",
    columns = c(id = "integer"),
    rules = list(dr_quality_rule("positive", ~ id > 0))
  )
  definition <- dr_product(
    "orders",
    data.frame(id = 1:2),
    code_version = "v1"
  ) |>
    dr_add_contract(declaration)
  first <- dr_publish(definition, to = lake, cache = TRUE)
  assets <- dr_registry(lake, "assets")
  declared <- assets[assets$id == declaration$id, ]
  alternate <- dr_execution_config(quality = "pointblank")
  resolved <- apply_execution_defaults(definition, alternate)
  expect_identical(resolved$contract, declaration)
  second <- dr_publish(
    definition,
    to = lake,
    execution = alternate,
    cache = TRUE
  )
  expect_identical(second$status, "published")
  expect_identical(
    dr_quality(first)$engine[dr_quality(first)$rule == "positive"],
    "r"
  )
  expect_identical(
    dr_quality(second)$engine[dr_quality(second)$rule == "positive"],
    "pointblank"
  )
  expect_identical(dr_collect(first), dr_collect(second))
  assets <- dr_registry(lake, "assets")
  expect_identical(assets[assets$id == declaration$id, ], declared)
  expect_identical(nrow(dr_registry(lake, "releases")), 2L)
  cached <- dr_publish(
    definition,
    to = lake,
    execution = alternate,
    cache = TRUE
  )
  expect_identical(cached$status, "cached")
  expect_identical(cached$release_id, second$release_id)
  changed <- declaration
  changed$rules <- list(dr_quality_rule("positive", ~ id > 1))
  condition <- tryCatch(
    dr_publish(
      dr_add_contract(definition, changed),
      to = lake,
      execution = alternate
    ),
    error = identity
  )
  expect_match(
    conditionMessage(condition),
    "Definition changed without a version bump"
  )
  expect_identical(dr_collect(first)$id, 1:2)
  expect_identical(dr_read_release(lake, "orders")$id, 1:2)
})

test_that("ingestion registers the declared contract before resolved checks", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("pointblank")
  lake <- dr_open_lake(withr::local_tempdir())
  withr::defer(dr_close_lake(lake))
  declaration <- dr_contract(
    "delivery_schema",
    columns = c(id = "integer"),
    rules = list(dr_quality_rule("positive", ~ id > 0))
  )
  definition <- dr_product("delivery", data.frame(id = 1L)) |>
    dr_add_contract(declaration)
  first <- dr_ingest(definition, lake)
  second <- dr_ingest(
    definition,
    lake,
    execution = dr_execution_config(quality = "pointblank")
  )
  expect_identical(second$status, "published")
  assets <- dr_registry(lake, "assets")
  expect_identical(
    assets$fingerprint[assets$id == declaration$id],
    fingerprint(declaration)
  )
  expect_identical(
    dr_quality(second)$engine[dr_quality(second)$rule == "positive"],
    "pointblank"
  )
  expect_identical(dr_collect(first)$id, 1L)
})

test_that("explicit product versions still protect execution definitions", {
  skip_if_not_installed("dataraft.lake")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("pointblank")
  lake <- dr_open_lake(withr::local_tempdir())
  withr::defer(dr_close_lake(lake))
  definition <- dr_product(
    "orders",
    data.frame(id = 1L),
    version = "1",
    code_version = "v1"
  ) |>
    dr_add_contract(dr_contract(
      "orders_schema",
      columns = c(id = "integer"),
      rules = list(dr_quality_rule("positive", ~ id > 0))
    ))
  first <- dr_publish(definition, to = lake, cache = TRUE)
  condition <- tryCatch(
    dr_publish(
      definition,
      to = lake,
      cache = TRUE,
      execution = dr_execution_config(quality = "pointblank")
    ),
    error = identity
  )
  expect_match(
    conditionMessage(condition),
    "Definition changed without a version bump: orders 1"
  )
  expect_identical(dr_collect(first)$id, 1L)
  expect_identical(nrow(dr_registry(lake, "releases")), 1L)
})
