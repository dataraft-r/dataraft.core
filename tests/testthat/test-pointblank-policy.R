test_that("native thresholds are evaluated independently for each pointblank segment", {
  skip_if_not_installed("pointblank")
  data <- data.frame(
    entity = rep(c("A", "B"), each = 100),
    amount = rep(1, 200)
  )
  data$amount[1:3] <- -1
  contract <- dr_contract(
    "amounts",
    "1",
    "Analytics",
    "Amounts",
    "One amount",
    c(entity = "character", amount = "numeric"),
    rules = list(
      dr_pointblank_checks(
        "positive",
        function(data) {
          pointblank::create_agent(
            data,
            actions = pointblank::action_levels(warn_at = 0.005, stop_at = 0.05)
          ) |>
            pointblank::col_vals_gte(
              "amount",
              0,
              segments = pointblank::vars(entity)
            )
        },
        policy = "agent"
      )
    )
  )
  quality <- dr_validate(data, contract, keep_agents = TRUE)
  checks <- quality[quality$engine == "pointblank", ]
  expect_equal(checks$status, c("warning", "passed"))
  expect_equal(checks$n_total, c(100, 100))
  expect_equal(length(unique(checks$segment)), 2L)
  expect_equal(grepl('"A"', checks$segment[[1]], fixed = TRUE), TRUE)
  expect_equal(dataraft.core:::quality_ok(quality), TRUE)
  data$amount[4:5] <- -1
  checks <- dr_validate(data, contract)
  expect_equal(
    checks$status[checks$engine == "pointblank"],
    c("failed", "passed")
  )
  expect_equal(dataraft.core:::quality_ok(checks), FALSE)
  expect_equal(is.null(attr(checks, "pointblank_agents")), TRUE)
})

test_that("native policy fails closed for inactive, errored and unconfigured checks", {
  skip_if_not_installed("pointblank")
  builders <- list(
    function(data) {
      pointblank::create_agent(data) |> pointblank::col_vals_gte("x", 0)
    },
    function(data) {
      pointblank::create_agent(
        data,
        actions = pointblank::action_levels(warn_at = 1)
      ) |>
        pointblank::col_vals_gte("x", 0, active = FALSE)
    },
    function(data) {
      pointblank::create_agent(
        data,
        actions = pointblank::action_levels(stop_at = 1)
      ) |>
        pointblank::col_vals_gte("missing", 0)
    }
  )
  for (build in builders) {
    contract <- dr_contract(
      "x",
      "1",
      "Analytics",
      "Values",
      "One value",
      c(x = "numeric"),
      rules = list(dr_pointblank_checks("check", build, policy = "agent"))
    )
    quality <- dr_validate(data.frame(x = 1), contract)
    expect_equal(dataraft.core:::quality_ok(quality), FALSE)
    expect_equal(any(quality$status %in% c("error", "not_checked")), TRUE)
  }
})

test_that("rule policy remains independent of native action levels", {
  skip_if_not_installed("pointblank")
  contract <- dr_contract(
    "x",
    "1",
    "Analytics",
    "Values",
    "One value",
    c(x = "numeric"),
    rules = list(dr_pointblank_checks(
      "check",
      function(data) {
        pointblank::create_agent(
          data,
          actions = pointblank::action_levels(stop_at = 1)
        ) |>
          pointblank::col_vals_gte("x", 0)
      },
      severity = "warning"
    ))
  )
  quality <- dr_validate(data.frame(x = c(-1, 1)), contract)
  expect_equal(quality$status[quality$engine == "pointblank"], "warning")
  expect_equal(dataraft.core:::quality_ok(quality), TRUE)
})

test_that("every native blocking action takes precedence across report layouts", {
  skip_if_not_installed("pointblank")
  report <- pointblank::get_agent_report
  flags <- NULL
  local_family_bindings(
    get_agent_report = function(...) {
      out <- report(...)
      out[intersect(c("W", "S", "E", "C"), names(out))] <- NULL
      for (name in names(flags)) {
        out[[name]] <- flags[[name]]
      }
      out
    },
    .package = "pointblank"
  )
  contract <- dr_contract(
    "amounts",
    "1",
    "Analytics",
    "Amounts",
    "One amount",
    c(amount = "numeric"),
    rules = list(dr_pointblank_checks(
      "positive",
      function(data) {
        pointblank::create_agent(
          data,
          actions = pointblank::action_levels(stop_at = 1)
        ) |>
          pointblank::col_vals_gte("amount", 0)
      },
      policy = "agent"
    ))
  )
  layouts <- list(
    list(W = FALSE, S = TRUE, E = FALSE, C = NA),
    list(W = TRUE, S = TRUE, E = NA, C = FALSE),
    list(W = FALSE, S = FALSE, E = TRUE, C = NA),
    list(W = FALSE, E = FALSE, C = TRUE),
    list(S = TRUE),
    list(W = TRUE, S = FALSE, E = FALSE, C = FALSE),
    list(W = FALSE, E = FALSE, C = FALSE),
    list(W = NA, S = NA, E = NA, C = NA)
  )
  actual <- vapply(
    layouts,
    function(layout) {
      flags <<- layout
      quality <- dr_validate(data.frame(amount = -1), contract)
      quality$status[quality$engine == "pointblank"]
    },
    character(1)
  )
  expect_equal(actual, c(rep("failed", 5), "warning", "passed", "error"))
})
