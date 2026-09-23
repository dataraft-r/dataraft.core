#' Define a delivery dependency graph
#'
#' Named functions describe the receipt, preparation and dbt steps once.
#' Each function argument names an input or another step. Steps execute in
#' dependency order; a failed step blocks its consumers. Results remain ordinary
#' R values, so existing [dataraft.lake::dr_ingest()], [dr_publish()] and dbt calls keep their meaning.
#' Keep report issuance outside the workflow as an explicit final decision.
#'
#' On correction, pass replacement `inputs` and the `previous` workflow result
#' to [dr_run()]. Unchanged successful branches are reused; changed inputs and their
#' consumers rerun. Failed branches retry on the next explicit call. Change
#' `code_version` whenever code, captured values or dependencies change. Use
#' `refresh` for named steps whose external sources changed without a new input.
#' This is sequential orchestration, not a scheduler or a distributed transaction.
#' Already committed steps stay committed if a later step fails.
#' @param ... Named step functions. Their arguments must name dependencies;
#'   defaults and `...` in step functions are not supported.
#' @param inputs Named list of initial input values.
#' @param code_version Explicit version of workflow code and dependencies.
#'   Required for every dependency graph.
#' @returns A workflow specification. Running it returns named `results`,
#'   effective `inputs`, and a step `status` table.
#' @export
#' @examples
#' flow <- dr_workflow(total = function(delivery) sum(delivery),
#'   inputs = list(delivery = c(10, 20)), code_version = "v1")
#' dr_run(flow)$results$total
dr_workflow <- function(
  ...,
  inputs = list(),
  code_version = NULL
) {
  steps <- list(...)

  scalar(code_version, "code_version")
  check_names <- function(x) {
    rlang::local_error_call(rlang::caller_env())
    is.list(x) &&
      (!length(x) ||
        (!is.null(names(x)) &&
          !anyNA(names(x)) &&
          all(nzchar(names(x))) &&
          !anyDuplicated(names(x))))
  }
  if (
    !length(steps) ||
      !check_names(steps) ||
      !check_names(inputs) ||
      length(intersect(names(steps), names(inputs))) ||
      !all(vapply(steps, is.function, logical(1)))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Supply uniquely named step functions and inputs with distinct names."
    )
  }
  dependencies <- lapply(steps, function(step) {
    args <- formals(step)
    if (is.null(args) && is.primitive(step)) {
      abort(
        subclass = "dataraft_error_definition",
        "Wrap primitive functions in an ordinary function."
      )
    }
    if (
      "..." %in%
        names(args) ||
        any(vapply(
          as.list(args),
          function(x) {
            !rlang::is_missing(x)
          },
          logical(1)
        ))
    ) {
      abort(
        subclass = "dataraft_error_definition",
        "Step arguments must be required named dependencies, without defaults or dots."
      )
    }
    names(args) %||% character()
  })
  if (!all(unlist(dependencies) %in% c(names(inputs), names(steps)))) {
    abort(
      subclass = "dataraft_error_definition",
      "Every step argument must name a workflow input or step."
    )
  }
  order <- character()
  remaining <- names(steps)
  while (length(remaining)) {
    ready <- remaining[vapply(
      dependencies[remaining],
      function(deps) {
        all(deps %in% c(names(inputs), order))
      },
      logical(1)
    )]
    if (!length(ready)) {
      abort(
        subclass = "dataraft_error_definition",
        "Workflow dependencies contain a cycle."
      )
    }
    order <- c(order, ready)
    remaining <- setdiff(remaining, ready)
  }
  structure(
    list(
      steps = steps,
      inputs = inputs,
      dependencies = dependencies,
      order = order,
      code_version = code_version
    ),
    class = "dr_workflow"
  )
}


#' @rdname dr_workflow
#' @param pipeline A workflow specification.
#' @param lake Unused; supply storage inside the relevant step definition.
#' @param previous Previous result from the same workflow input names.
#' @param refresh Step names to rerun even when their inputs are unchanged.
#' @param stop_on_failure Signal an error after collecting step status. Set
#'   `FALSE` to inspect failures directly; errors also retain `condition$result`.
#' @export
dr_run.dr_workflow <- function(
  pipeline,
  lake = NULL,
  inputs = list(),
  previous = NULL,
  refresh = character(),
  stop_on_failure = TRUE,
  ...
) {
  rlang::check_dots_empty()
  pipeline <- do.call(
    dr_workflow,
    c(
      pipeline$steps,
      list(inputs = pipeline$inputs, code_version = pipeline$code_version)
    )
  )
  flag(stop_on_failure, "stop_on_failure")
  if (!is.null(lake)) {
    abort(
      subclass = "dataraft_error_definition",
      "Configure storage in the workflow's step definitions."
    )
  }
  if (
    !is.list(inputs) ||
      (length(inputs) &&
        (is.null(names(inputs)) ||
          anyNA(names(inputs)) ||
          anyDuplicated(names(inputs)) ||
          !all(names(inputs) %in% names(pipeline$inputs))))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Replacement inputs must name existing workflow inputs uniquely."
    )
  }
  if (
    !is.character(refresh) ||
      anyNA(refresh) ||
      !all(refresh %in% names(pipeline$steps))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "refresh must name existing workflow steps."
    )
  }
  if (
    !is.null(previous) &&
      (!inherits(previous, "dr_workflow_result") ||
        !identical(names(previous$inputs), names(pipeline$inputs)))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "previous must be a workflow result with the same input names."
    )
  }
  effective <- if (is.null(previous)) pipeline$inputs else previous$inputs
  if (!is.null(previous)) {
    revised <- names(pipeline$inputs)[
      !vapply(
        names(pipeline$inputs),
        function(name) {
          identical(pipeline$inputs[[name]], previous$definition$inputs[[name]])
        },
        logical(1)
      )
    ]
    effective[revised] <- pipeline$inputs[revised]
  }
  effective[names(inputs)] <- inputs
  same_code <- !is.null(previous) && identical(previous$definition, pipeline)
  changed <- if (same_code) {
    names(effective)[
      !vapply(
        names(effective),
        function(name) {
          identical(effective[[name]], previous$inputs[[name]])
        },
        logical(1)
      )
    ]
  } else {
    c(names(effective), names(pipeline$steps))
  }
  changed <- union(changed, refresh)
  results <- list()
  states <- list()
  errors <- list()
  failed <- character()
  for (name in pipeline$order) {
    deps <- pipeline$dependencies[[name]]
    if (any(deps %in% failed)) {
      state <- "skipped"
      failed <- c(failed, name)
    } else if (
      same_code &&
        !name %in% changed &&
        !any(deps %in% changed) &&
        previous$status$status[match(name, previous$status$step)] %in%
          c("completed", "reused")
    ) {
      results[name] <- previous$results[name]
      state <- "reused"
    } else {
      changed <- union(changed, name)
      value <- tryCatch(
        {
          value <- do.call(pipeline$steps[[name]], c(effective, results)[deps])
          if (
            (inherits(value, "dr_run_result") &&
              !value$status %in% c("completed", "published", "cached")) ||
              (inherits(value, "dr_dbt_result") && !isTRUE(value$success))
          ) {
            abort(
              subclass = "dataraft_error_definition",
              "Step did not complete successfully.",
              result = value
            )
          }
          list(value = value)
        },
        error = function(e) {
          errors[[name]] <<- e
          NULL
        }
      )
      if (is.null(value)) {
        state <- "failed"
        failed <- c(failed, name)
      } else {
        results[name] <- value
        state <- "completed"
      }
    }
    states[[name]] <- tibble::tibble(
      step = name,
      status = state,
      success = state %in% c("completed", "reused")
    )
  }
  out <- structure(
    list(
      results = results,
      inputs = effective,
      status = dplyr::bind_rows(states),
      errors = errors,
      definition = pipeline
    ),
    class = "dr_workflow_result"
  )
  if (length(failed) && stop_on_failure) {
    abort(
      subclass = "dataraft_error_definition",
      "Workflow incomplete. Inspect dr_status(condition$result) and condition$result$errors; successful steps remain available for an explicit retry.",
      "dr_workflow_failed",
      result = out
    )
  }
  out
}


#' @export
print.dr_workflow <- function(x, ...) {
  cat("<workflow> ", x$code_version, "\n", sep = "")
  print(tibble::tibble(
    step = x$order,
    inputs = vapply(
      x$dependencies[x$order],
      function(deps) paste(deps, collapse = ", "),
      character(1)
    )
  ))
  invisible(x)
}


#' @export
print.dr_workflow_result <- function(x, ...) {
  cat("<workflow result>\n")
  print(x$status)
  cat("Step outputs: $results; retained local conditions: $errors.\n")
  invisible(x)
}
