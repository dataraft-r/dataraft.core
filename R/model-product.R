# A model keeps the ordinary product fields and specializes execution/output.
new_model_product <- function(x, data, contracts) {
  rlang::local_error_call(rlang::caller_env())
  need("dm")
  tables <- as.list(data)
  if (!length(tables)) {
    abort(
      subclass = "dataraft_error_definition",
      "A model product needs at least one table."
    )
  }
  invisible(lapply(names(tables), asset_id))
  if (is.null(contracts)) {
    contracts <- list()
  }
  if (
    !is.list(contracts) ||
      (length(contracts) &&
        (is.null(names(contracts)) ||
          anyNA(names(contracts)) ||
          anyDuplicated(names(contracts)) ||
          any(!names(contracts) %in% names(tables))))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "contracts must be named by model table, without duplicates."
    )
  }
  pk <- dm::dm_get_all_pks(data)
  fk <- dm::dm_get_all_fks(data)
  x$primary_keys <- stats::setNames(lapply(pk$pk_col, as.character), pk$table)
  x$foreign_keys <- lapply(seq_len(nrow(fk)), function(i) {
    list(
      table = fk$child_table[[i]],
      columns = as.character(fk$child_fk_cols[[i]]),
      ref_table = fk$parent_table[[i]],
      ref_columns = as.character(fk$parent_key_cols[[i]])
    )
  })
  x$sources <- stats::setNames(
    lapply(names(tables), function(name) {
      dr_product(
        paste(x$id, name, sep = "."),
        tables[[name]],
        contract = contracts[[name]],
        source_name = name
      )
    }),
    names(tables)
  )
  class(x) <- c("dr_model_product", class(x))
  x
}


#' @export
dr_validate.dr_model_product <- function(
  data,
  contract = NULL,
  ...,
  .write = TRUE
) {
  rlang::check_dots_empty()
  if (
    !is.null(contract) ||
      !is.null(data$contract) ||
      length(data$quality) ||
      length(data$transforms) ||
      length(data$catalogs)
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Configure checks and transformations on the model's table products. Select a table with dr_product(..., table = ) after dr_run(write = FALSE, stop_on_failure = FALSE) or dr_publish()."
    )
  }
  if (
    !length(data$sources) ||
      anyDuplicated(names(data$sources)) ||
      !all(vapply(
        data$sources,
        function(x) {
          inherits(x, "dr_product") &&
            !inherits(x, "dr_model_product")
        },
        logical(1)
      ))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Model members must be named table products."
    )
  }
  invisible(lapply(data$sources, dr_validate, .write = .write))
  if (.write && !is.null(data$target)) {
    if (
      !isTRUE(dr_capabilities(data$target)$transactions) ||
        !isTRUE(dr_capabilities(data$target)$immutable) ||
        length(data$target$partition_by)
    ) {
      abort(
        subclass = "dataraft_error_definition",
        "Model products publish complete snapshots to a lake target."
      )
    }
    dr_check_component(data$target)
  }
  data
}


#' @export
dr_inspect.dr_model_product <- function(x, ...) {
  list(
    id = x$id,
    version = x$version,
    kind = "model_product",
    tables = lapply(x$sources, dr_inspect),
    primary_keys = x$primary_keys,
    foreign_keys = x$foreign_keys,
    target = dr_inspect(x$target)
  )
}


#' @export
print.dr_model_product <- function(x, ...) {
  cat(
    "<model product>",
    x$id,
    "| tables:",
    paste(names(x$sources), collapse = ", "),
    "\n"
  )
  invisible(x)
}


#' @export
dr_run.dr_model_product <- function(
  pipeline,
  lake = NULL,
  stop_on_failure = TRUE,
  execution = NULL,
  data = NULL,
  sources = NULL,
  previous = NULL,
  write = TRUE,
  ...
) {
  rlang::check_dots_empty()
  flag(stop_on_failure, "stop_on_failure")
  flag(write, "write")
  if (!is.null(data)) {
    abort(
      subclass = "dataraft_error_definition",
      "Replace model tables with sources = list(table_name = delivery)."
    )
  }
  withr::local_options(dataraft.verify_determinism = !write)
  x <- pipeline
  if (!is.null(sources)) {
    x <- replace_sources_list(x, sources)
  }
  execution <- product_execution(x, execution)
  x <- apply_execution_defaults(x, execution)
  if (!is.null(lake)) {
    x <- dr_set_target(x, lake)
  }
  x <- dr_validate(x, .write = write)
  result <- run_result(uid(), "completed")
  class(result) <- c("dr_model_result", class(result))
  result$asset <- x$id
  result$primary_keys <- x$primary_keys
  result$foreign_keys <- x$foreign_keys
  result$members <- lapply(x$sources, function(member) {
    dr_run(write = FALSE, stop_on_failure = FALSE, member)
  })
  checks <- lapply(names(result$members), function(name) {
    out <- dr_quality(result$members[[name]])
    out$rule <- paste(name, out$rule, sep = "/")
    out
  })
  result$quality <- dplyr::bind_rows(checks)
  good <- vapply(
    result$members,
    function(m) m$status == "completed",
    logical(1)
  )
  if (!all(good)) {
    result$status <- "blocked"
  } else {
    candidate <- dm::dm(!!!lapply(result$members, dr_collect))
    checked <- tryCatch(
      dm_keys(candidate, x$primary_keys, x$foreign_keys, TRUE),
      error = identity
    )
    if (inherits(checked, "error")) {
      result$status <- "blocked"
      result$error <- checked
      result$quality <- dplyr::bind_rows(
        result$quality,
        quality_row(
          "relationships",
          "failed",
          engine = "dm",
          message = "Model keys or references failed. Inspect result$error$checks."
        )
      )
    } else {
      result$data <- checked
      result$quality <- dplyr::bind_rows(
        result$quality,
        quality_row("relationships", "passed", engine = "dm")
      )
    }
  }
  result$validation_status <- if (any(result$quality$status == "unvalidated")) {
    "unvalidated"
  } else if (!quality_ok(result$quality)) {
    "failed"
  } else if (any(result$quality$status == "warning")) {
    "warning"
  } else {
    "passed"
  }
  if (write && result$status == "completed" && !is.null(x$target)) {
    result <- tryCatch(
      dr_publish_model_result(x$target, x, result, previous),
      error = function(e) {
        result$status <- "error"
        result$error <- e
        if (stop_on_failure) {
          abort(
            subclass = failure_subclass(result),
            "Model publication failed. Inspect condition$result$error locally.",
            class = class(e)[[1]],
            parent = e,
            result = result
          )
        }
        result
      }
    )
  } else if (!is.null(previous) && is.null(x$target)) {
    abort(
      subclass = "dataraft_error_definition",
      "previous is available only when publishing to a lake."
    )
  }
  if (stop_on_failure && result$status == "blocked") {
    abort(
      subclass = failure_subclass(result),
      paste(
        "Model",
        x$id,
        "failed checks. Use result <- dr_last_failure() and inspect dr_quality_report(result) and dr_quality_errors(result)."
      ),
      "dr_model_failed",
      result = result
    )
  }
  result
}


#' @export
collect.dr_model_result <- function(x, ...) {
  rlang::check_dots_empty()
  if (!x$status %in% c("completed", "published")) {
    abort(
      subclass = failure_subclass(x),
      "The model failed checks. Use result <- dr_last_failure(), then inspect dr_quality_report(result), dr_quality_rows(result) and dr_quality_errors(result).",
      result = x,
      checks = x$quality,
      parent = run_result_parent(x)
    )
  }
  if (!is.null(x$data)) {
    return(x$data)
  }
  dr_with_release_backend(x, function(lake) {
    dr_read_model_release(lake, x$asset, x$release_id)
  })
}


model_member_result <- function(x, table) {
  rlang::local_error_call(rlang::caller_env())
  scalar(table, "table")
  if (
    !inherits(x, "dr_model_result") ||
      !x$status %in% c("completed", "published")
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Select a table from a successful dr_run(write = FALSE, stop_on_failure = FALSE) or dr_publish() model result."
    )
  }
  if (!table %in% names(x$members)) {
    abort(
      subclass = "dataraft_error_definition",
      paste(
        "Unknown model table. Choose:",
        paste(names(x$members), collapse = ", ")
      )
    )
  }
  x$members[[table]]
}


#' @export
explain.dr_model_product <- function(x, ...) {
  rlang::local_error_call(rlang::caller_env())
  print(x)
  cat(
    "Define table checks with contracts = list(...). Replace deliveries by table name.\n"
  )
  invisible(dr_inspect(x))
}
