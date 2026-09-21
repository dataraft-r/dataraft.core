#' Execute a product using a target adapter
#'
#' Most extensions only need [dr_write_target()]: the default executor reads the
#' source, applies transforms, checks the candidate, then calls the writer.
#' Implement this generic only when execution needs different resource or
#' transaction semantics. The lake adapter compiles the product into the
#' existing governed ingestion lifecycle. Methods return `dr_run_result`.
#' @param target Target adapter, or `NULL` for in-memory execution.
#' @param product Validated product specification.
#' @param ... Execution options. Target adapters must reject unsupported options.
#' @returns A run result containing execution evidence and an output reference.
#' @examples
#' product <- dr_product("orders") |> dr_add_source(data.frame(id = 1:2))
#' dataraft.core::dr_execute_target(NULL, dr_validate(product))
#' @export
#' @keywords internal
#' @name dr_execute_target
#' @title Target execution protocol
dr_execute_target <- function(target, product, ...) {
  UseMethod("dr_execute_target")
}


#' Write checked data using a target adapter
#'
#' The default executor calls this only after its contract and quality gate
#' pass. Methods receive a data frame and descriptive run context. A writer
#' must raise an error on failure and must never silently report success.
#' It owns its destination's atomicity and cleanup. Return a list describing
#' the output, without input rows, credentials or open connections. File and
#' database adapters can return paths or stable table identifiers.
#' @param target Target adapter. `NULL` retains data in the run result.
#' @param data Checked data frame or tibble.
#' @param context List with run ID, product ID, contract and metadata.
#' @param ... Reserved for adapter-specific options.
#' @returns A list describing the written output.
#' @export
#' @examples
#' dr_write_target(NULL, data.frame(id = 1L), list(product = "orders"))
dr_write_target <- function(target, data, context, ...) {
  UseMethod("dr_write_target")
}

#' @export
dr_write_target.NULL <- function(target, data, context, ...) {
  list(type = "memory", rows = count_rows(data))
}

#' @export
dr_write_target.default <- function(target, data, context, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "This target needs a dr_write_target() method."
  )
}


#' Publish automatically generated metadata
#'
#' Catalog adapters receive product metadata after execution according to their
#' declared lifecycle support. A function is sufficient for a small integration.
#' Its return value is ignored. Metadata
#' delivery is separate from data publication: a catalog failure is recorded
#' as a warning and does not pretend that a committed data release was undone.
#'
#' [dataraft.dbt::dr_dbt_build()] and [dataraft.dbt::dr_dbt_test()] also accept a catalog function, or an S3
#' adapter with `dr_capabilities(x)$metadata_inputs = "dr_dbt_result"`. These
#' receive the full dbt result, including local artifact paths, the manifest,
#' and dbt stdout/stderr. Callback authors control what is transmitted; these
#' diagnostics can contain SQL or database messages. Valid artifacts from
#' failed dbt tests can still be delivered without changing the dbt outcome.
#' [dataraft.catalog::dr_catalog_openmetadata_dbt()] delegates to the existing OpenMetadata ingestion
#' engine and returns a retryable, credential-free delivery receipt.
#' @param catalog Function or catalog adapter.
#' @param metadata Descriptive product run metadata without input rows or
#'   connections, or a `dr_dbt_result` for a catalog supporting dbt artifacts.
#' @param ... Reserved for adapter-specific options.
#' @returns The adapter's result; no specific value is required. Functions are
#'   called invisibly. The OpenMetadata dbt adapter returns a delivery receipt.
#' @export
#' @examples
#' received <- NULL
#' dr_publish_metadata(function(metadata) received <<- metadata,
#'   list(product = "orders", rows = 2L))
dr_publish_metadata <- function(catalog, metadata, ...) {
  UseMethod("dr_publish_metadata")
}

#' @export
dr_publish_metadata.function <- function(catalog, metadata, ...) {
  invisible(catalog(metadata))
}

#' @export
dr_publish_metadata.default <- function(catalog, metadata, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "This catalog needs a dr_publish_metadata() method."
  )
}


#' Execute and publish a product with sensible local defaults
#'
#' Use [dr_trial()] to try a product without configured framework writers.
#' `dr_run()` executes its full configuration, including targets and catalogs.
#' `dr_publish()` adds a local lake target when none was supplied. A new folder
#' uses DuckDB; an existing folder retains its saved backend. [dataraft.lake::dr_target_lake()]
#' accepts other lake configurations. Execution automatically validates the
#' definition before source acquisition.
#' Use [dr_collect()] to obtain the output as an ordinary tibble.
#' @param x Composed product, data frame, or successful dbt build.
#' @param name Product name for a data frame, or an exact or unambiguous model
#'   name for a dbt build. Omit it for an already named product.
#' @param to Optional target, replacing an already configured target. Defaults
#'   to the `dataraft` folder in the working directory when the product has none.
#'   For dbt builds it defaults to the project's configured lake.
#' @param layer Optional publication layer for a lake target.
#' @param execution Optional [dr_execution_config()] defaults for products,
#'   overriding defaults stored by `dr_product(execution = )`.
#' @param ... Execution options, including `data` or `sources` for new deliveries
#'   as described in [dr_run()], `stop_on_failure` and, for lake
#'   targets, `business_date`, `notify`, `cache` and `previous`. Supply a previous
#'   publication to reject stale corrections if the destination has changed. dbt builds accept
#'   [dataraft.dbt::dr_dbt_publish()] options such as `contract`, `asset` and `layer`.
#' @returns A run result. An exception on failure includes `condition$result`.
#' @export
#' @examplesIf requireNamespace("dataraft.lake", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE)
#' root <- tempfile("dataraft-")
#' result <- dr_product("orders") |>
#'   dr_add_source(data.frame(id = 1:2)) |>
#'   dr_publish(to = root)
#' dr_collect(result)
#' unlink(root, recursive = TRUE)
dr_publish <- function(x, name = NULL, to = NULL, ...) {
  UseMethod("dr_publish")
}

#' @rdname dr_publish
#' @export
dr_publish.data.frame <- function(x, name = NULL, to = NULL, ...) {
  if (is.null(name)) {
    abort(
      subclass = "dataraft_error_definition",
      "Supply name when publishing a data frame, for example 'orders'."
    )
  }
  dr_publish(dr_product(name, x), to = to, ...)
}

#' @rdname dr_publish
#' @export
dr_publish.dr_product <- function(
  x,
  name = NULL,
  to = NULL,
  layer = NULL,
  execution = NULL,
  ...
) {
  if (!is.null(name)) {
    abort(
      subclass = "dataraft_error_definition",
      "The product already has a name. Omit name or create dr_product(name)."
    )
  }
  execution <- product_execution(x, execution)
  x <- editable_product(x)
  if (!is.null(to)) {
    x <- dr_set_target(x, to)
    if (
      is.null(layer) &&
        !is.null(execution$layer) &&
        !inherits(to, "dr_lake_target")
    ) {
      layer <- execution$layer
    }
  }
  if (is.null(x$target)) {
    x <- dr_set_target(x, execution$to %||% "dataraft")
    layer <- layer %||% execution$layer
  }
  if (!is.null(layer)) {
    if (!inherits(x$target, "dr_lake_target")) {
      abort(
        subclass = "dataraft_error_definition",
        "layer is available only for lake publication targets."
      )
    }
    x$target$layer <- ident(layer)
  }
  dr_run(x, execution = execution, ...)
}

#' @export
dr_publish.default <- function(x, name = NULL, to = NULL, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "Publish a product, data frame or successful dbt build."
  )
}


#' Collect the output of a successful run
#'
#' In-memory results retain their ordinary table. Lake results read their exact
#' published release, opening and closing a read-only connection if needed.
#' Custom target results retain their checked table in memory; their output
#' descriptor is also available in `$outputs`. For an append target this is
#' the submitted batch, not the complete mutable destination. The run metadata
#' distinguishes `submitted_rows` from output `rows`; candidate quality may
#' describe the complete destination including previously stored rows.
#' @param x Run result, data frame or lazy table.
#' @param ... Arguments passed to dplyr when collecting retained data. Lake
#'   results collect the complete pinned release and accept no extra arguments.
#' @returns An ordinary tibble for a table product, or a dm for a model
#'   product. Failed or blocked runs cannot be collected.
#' @name dr_collect
#' @importFrom dplyr collect
#' @export
#' @examples
#' dr_product("orders") |> dr_add_source(data.frame(id = 1:2)) |>
#'   dr_run() |> dr_collect()
dr_collect <- function(x, ...) dplyr::collect(x, ...)


#' @rdname dr_collect
#' @export
collect.dr_product <- function(x, ...) {
  abort(
    c(
      "A product is a definition, not an executed result.",
      i = "Run result <- dr_trial(x), then dr_collect(result). Add a source with dr_add_source() if needed."
    ),
    subclass = "dataraft_error_definition"
  )
}

#' @rdname dr_collect
#' @export
collect.dr_product_workflow <- collect.dr_product


#' @rdname dr_collect
#' @export
collect.dr_run_result <- function(x, ...) {
  if (!x$status %in% c("completed", "published", "cached")) {
    abort(
      subclass = failure_subclass(x),
      c(
        run_result_message(x),
        i = "Use result <- dr_last_failure(), then inspect dr_quality_report(result) and dr_quality_rows(result).",
        i = "Inspect dr_quality_errors(result) for locally retained rule exceptions."
      ),
      result = x,
      checks = x$quality,
      parent = run_result_parent(x)
    )
  }
  if (!is.null(x$data)) {
    return(tibble::as_tibble(dplyr::collect(x$data, ...)))
  }
  rlang::check_dots_empty()
  if (!is.null(x$output_lake) && DBI::dbIsValid(x$output_lake$con)) {
    return(dataraft.lake::dr_read_release(x$output_lake, x$asset, x$release_id))
  }
  if (!is.null(x$output_config)) {
    config <- x$output_config
    config$read_only <- TRUE
    lake <- dataraft.lake::dr_connect_lake(config)
    on.exit(dataraft.lake::dr_close_lake(lake), add = TRUE)
    return(dataraft.lake::dr_read_release(lake, x$asset, x$release_id))
  }
  abort(
    subclass = "dataraft_error_definition",
    "This result has no output reference."
  )
}


#' @export
dr_run.dr_product <- function(
  pipeline,
  lake = NULL,
  stop_on_failure = TRUE,
  evidence = getOption("dataraft.evidence"),
  .context = NULL,
  execution = NULL,
  data = NULL,
  sources = NULL,
  ...
) {
  object <- replace_execution_sources(pipeline, data, sources)
  execution <- if (is.null(.context)) {
    product_execution(object, execution)
  } else {
    execution
  }
  if (!is.null(evidence)) {
    scalar(evidence, "evidence")
  }
  flag(stop_on_failure, "stop_on_failure")
  if (!is.null(lake)) {
    object <- dr_set_target(object, lake)
  }
  object <- apply_execution_defaults(object, execution)
  object <- dr_validate(object)
  context <- .context %||% new_product_context(evidence)
  if (exists(object$id, context$results, inherits = FALSE)) {
    return(get(object$id, context$results, inherits = FALSE))
  }
  attr(object, "dr_run_context") <- context
  started <- now()
  warnings <- list()
  result <- tryCatch(
    withCallingHandlers(
      dr_execute_target(object$target, object, ...),
      warning = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      if (
        inherits(e$result, "dr_run_result") &&
          !inherits(e, "dr_dependency_failed")
      ) {
        return(e$result)
      }
      x <- run_result(uid(), "error")
      x$error <- e
      x
    }
  )
  if (
    !inherits(result, "dr_run_result") ||
      !is.character(result$run_id) ||
      length(result$run_id) != 1L ||
      is.na(result$run_id) ||
      !nzchar(result$run_id) ||
      !is.character(result$status) ||
      length(result$status) != 1L ||
      is.na(result$status) ||
      !result$status %in%
        c("completed", "published", "cached", "blocked", "error", "missing")
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "The target executor must return a dr_run_result with a run ID and supported status."
    )
  }
  result$asset <- object$id
  result$started_at <- result$started_at %||% started
  result$finished_at <- result$finished_at %||% now()
  result$backend <- result$backend %||% dr_inspect(object$target)$type
  result$warnings <- result$warnings %||% character()
  result$warning_conditions <- warnings
  if (length(warnings)) {
    result$warnings <- c(
      result$warnings,
      paste0(
        "Execution produced ",
        length(warnings),
        " warning(s); inspect result$warning_conditions locally."
      )
    )
  }
  result$metadata <- c(
    result$metadata %||% list(),
    list(
      run_id = result$run_id,
      product = object$id,
      definition = dr_inspect(object),
      code_version = object$code_version,
      started_at = result$started_at,
      finished_at = result$finished_at,
      status = result$status,
      backend = result$backend,
      quality = canonical(result$quality),
      inputs = result$inputs,
      outputs = result$outputs
    )
  )
  result$lifecycle <- tibble::tibble(
    state = c("defined", "validated", "planned", "running", result$status),
    at = c(
      NA_character_,
      started,
      started,
      result$started_at,
      result$finished_at
    )
  )
  result <- finalize_product_run(result, object, evidence)
  assign(object$id, result, context$results)
  if (length(result$warnings)) {
    rlang::warn(
      result$warnings,
      class = if (length(warnings)) {
        "dr_execution_warning"
      } else {
        "dr_catalog_warning"
      }
    )
  }
  if (
    stop_on_failure && !result$status %in% c("completed", "published", "cached")
  ) {
    abort(
      subclass = failure_subclass(result),
      paste(
        run_result_message(result),
        "For diagnosis, rerun with stop_on_failure = FALSE and save the result. Inspect dr_quality_report(result) and dr_quality_rows(result)."
      ),
      "dr_run_failed",
      result = result,
      checks = result$quality,
      n_failed = stats::setNames(result$quality$n_failed, result$quality$rule),
      parent = run_result_parent(result)
    )
  }
  result
}


#' @export
#' @noRd
dr_execute_target.default <- function(target, product, ...) {
  rlang::check_dots_empty()
  run <- uid()
  started <- now()
  quality <- NULL
  input <- NULL
  transform_metadata <- list()
  out <- tryCatch(
    {
      acquired <- read_product_sources(product)
      data <- acquired$data
      input <- acquired$inputs
      lineage_recipe <- dr_recipe()
      lineage_recipe$steps <- product$transforms
      column_lineage <- if (is.data.frame(data) || inherits(data, "tbl_lazy")) {
        dr_column_lineage(lineage_recipe, colnames(data))
      } else {
        list(complete = FALSE, fields = list(), reason = "Multiple sources")
      }
      for (name in names(product$transforms)) {
        data <- apply_product_transform(
          product$transforms[[name]],
          data,
          name,
          sources = acquired$transform_sources[[name]]
        )
        details <- attr(data, "dr_transform_metadata")
        if (!is.null(details)) {
          transform_metadata[[name]] <- details
        }
        attr(data, "dr_transform_metadata") <- NULL
      }
      data <- table_result(data, "The final transformation")
      contract <- product_contract(product, data)
      partition <- prepare_quality_candidate(data, contract)
      data <- partition$data
      quality <- partition$quality
      if (!quality_ok(quality)) {
        blocked <- run_result(run, "blocked", quality = quality)
        blocked$quarantine <- partition$quarantine
        blocked$diagnostic <- list(data = data, contract = contract)
        blocked
      } else {
        metadata <- list(
          product = product$id,
          column_lineage = column_lineage,
          transformations = transform_metadata,
          schema = infer_column_types(data),
          rows = count_rows(data),
          contract = canonical(contract),
          lineage = list(
            inputs = input,
            to = product$id
          )
        )
        output <- dr_write_target(
          target,
          data,
          list(
            run_id = run,
            product = product$id,
            contract = contract,
            metadata = metadata
          )
        )
        if (!is.list(output)) {
          abort(
            subclass = "dataraft_error_definition",
            "The target writer must return an output descriptor list."
          )
        }
        if (!is.null(output$candidate_quality)) {
          if (
            !is.data.frame(output$candidate_quality) ||
              !quality_ok(output$candidate_quality)
          ) {
            abort(
              subclass = "dataraft_error_definition",
              "The writer returned invalid or failing candidate quality evidence."
            )
          }
          quarantine_evidence <- quality[
            quality$stage == "quarantine",
            ,
            drop = FALSE
          ]
          quality <- dplyr::bind_rows(
            output$candidate_quality,
            quarantine_evidence
          )
        }
        metadata$submitted_rows <- metadata$rows
        metadata$rows <- output$rows %||% metadata$rows
        metadata$schema <- output$schema %||% metadata$schema
        result <- run_result(
          run,
          if (is.null(target)) "completed" else "published",
          quality = quality
        )
        result$quarantine <- partition$quarantine
        result$data <- data
        result$outputs <- output
        result$metadata <- metadata
        result
      }
    },
    error = function(e) {
      blocked <- inherits(e, "dr_target_quality_failed")
      if (blocked && !is.null(e$quality)) {
        quality <- e$quality
      }
      result <- run_result(
        run,
        if (blocked) "blocked" else "error",
        quality = quality
      )
      result$error <- e
      result
    }
  )
  out$started_at <- started
  out$finished_at <- now()
  out$inputs <- input
  out
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name apply_product_transform

apply_product_transform <- function(transform, data, name, sources = list()) {
  rlang::local_error_call(rlang::caller_env())
  tryCatch(
    table_result(
      dr_execute_transform(transform, data, sources = sources),
      paste0("Transformation `", name, "`")
    ),
    error = function(e) {
      abort(
        subclass = "dataraft_error_definition",
        paste0("Transformation `", name, "` failed. ", conditionMessage(e)),
        "dr_transform_failed",
        parent = e
      )
    }
  )
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name effective_product_contract

effective_product_contract <- function(product) {
  rlang::local_error_call(rlang::caller_env())
  contract <- product$contract
  rules <- product$execution_contract_rules
  if (is.null(contract) || is.null(rules)) {
    return(contract)
  }
  # Keep the user's registered declaration immutable. A resolved execution is
  # a separate, content-addressed check definition, including its actual engine.
  contract$declared_contract <- list(
    id = contract$id,
    version = contract$version,
    fingerprint = fingerprint(contract)
  )
  contract$rules <- rules
  contract$id <- paste0(contract$id, ".execution")
  contract$version <- paste0("checks-", fingerprint(contract))
  contract
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name product_contract

product_contract <- function(product, data) {
  rlang::local_error_call(rlang::caller_env())
  contract <- effective_product_contract(product) %||%
    automatic_schema(product$id, automatic_types(infer_column_types(data)))
  combine_quality(contract, product$quality)
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name combine_quality

combine_quality <- function(contract, rules) {
  rlang::local_error_call(rlang::caller_env())
  if (!length(rules)) {
    return(contract)
  }
  contract$id <- paste0(contract$id, ".quality")
  contract$rules <- c(contract$rules, rules)
  contract$version <- paste0("checks-", fingerprint(contract))
  contract
}


new_product_context <- function(evidence = NULL) {
  rlang::local_error_call(rlang::caller_env())
  context <- new.env(parent = emptyenv())
  context$results <- new.env(parent = emptyenv())
  context$sources <- list()
  context$evidence <- evidence
  context
}


result_data <- function(result) {
  rlang::local_error_call(rlang::caller_env())
  if (!result$status %in% c("completed", "published", "cached")) {
    abort(
      subclass = "dataraft_error_definition",
      "An upstream product did not complete successfully.",
      "dr_dependency_failed",
      result = result
    )
  }
  result$data %||% dr_collect(result)
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name read_product_sources

read_product_sources <- function(product, lake = NULL, on_input = NULL) {
  rlang::local_error_call(rlang::caller_env())
  context <- attr(product, "dr_run_context") %||% new_product_context()
  if (!is.null(lake)) {
    previous_read_lake <- context$read_lake
    context$read_lake <- lake
    withr::defer(context$read_lake <- previous_read_lake)
  }
  sources <- product_sources(product)
  tables <- vector("list", length(sources))
  names(tables) <- names(sources)
  inputs <- vector("list", length(tables))
  archives <- list()
  for (i in seq_along(sources)) {
    name <- names(sources)[[i]]
    source <- sources[[i]]
    reference <- NULL
    reported <- FALSE
    if (inherits(source, "dr_product")) {
      upstream <- dr_run(
        source,
        .context = context,
        evidence = context$evidence,
        stop_on_failure = FALSE
      )
      data <- result_data(upstream)
      reference <- list(
        asset = source$id,
        release_id = upstream$release_id,
        run_id = upstream$run_id
      )
      description <- list(type = "product", id = source$id)
    } else {
      match <- which(vapply(
        context$sources,
        function(item) {
          identical(item$source, source) && identical(item$lake, lake)
        },
        logical(1)
      ))
      if (length(match)) {
        item <- context$sources[[match[[1]]]]
        data <- item$data
        archive <- item$archive
      } else {
        archive <- NULL
        if (!is.null(lake) && inherits(source, "dr_source")) {
          landed <- dataraft.lake::dr_internal_land_source(lake, source)
          archive <- list(
            source = source$id,
            source_version = source$version,
            fingerprint = landed$hash,
            original_name = basename(source$path),
            landed_path = landed$uri,
            received_at = landed$received_at
          )
          if (!is.null(on_input)) {
            on_input(name, archive)
            reported <- TRUE
          }
          data <- source$reader(landed$path)
          if (
            !identical(
              digest::digest(
                file = landed$path,
                algo = "sha256",
                serialize = FALSE
              ),
              landed$hash
            )
          ) {
            abort(
              subclass = "dataraft_error_definition",
              "Reader modified immutable landing input."
            )
          }
          archive <- list(
            source = source$id,
            source_version = source$version,
            fingerprint = landed$hash,
            original_name = basename(source$path),
            landed_path = landed$uri,
            received_at = landed$received_at
          )
        } else {
          data <- if (identical(class(source), "dr_release_source")) {
            dataraft.lake::dr_internal_read_release_source(
              source,
              lake %||% context$read_lake
            )
          } else {
            dr_read_source(source)
          }
        }
        context$sources[[length(context$sources) + 1L]] <-
          list(source = source, lake = lake, data = data, archive = archive)
      }
      reference <- attr(data, "dr_input_reference")
      if (!is.null(archive)) {
        archives[[name]] <- archive
        if (!reported && !is.null(on_input)) on_input(name, archive)
      }
      description <- inspect_source(source)
      provenance <- attr(data, "dr_source_metadata")
      if (!is.null(provenance)) description$provenance <- provenance
    }
    data <- table_result(data, paste0("Source `", name, "`"))
    lazy <- is_lazy_table(data)
    pinned <- length(reference$release_id) == 1L &&
      !is.na(reference$release_id) &&
      nzchar(reference$release_id)
    if (!is.null(on_input) && !is.null(reference$asset)) {
      on_input(
        name,
        list(
          source = reference$asset,
          source_version = if (pinned) {
            reference$release_id
          } else {
            reference$run_id %||% "unversioned"
          },
          fingerprint = reference$hash %||%
            if (pinned) reference$release_id else reference$run_id %||% "",
          original_name = "",
          landed_path = "",
          received_at = now()
        )
      )
    }
    fingerprint <- if (pinned) {
      reference$release_id
    } else if (lazy) {
      digest::digest(description, algo = "sha256")
    } else {
      digest::digest(data, algo = "sha256")
    }
    inputs[[i]] <- tibble::tibble(
      name = name,
      source = list(description),
      rows = count_rows(data),
      fingerprint = fingerprint,
      hash_kind = if (pinned) {
        "release"
      } else if (lazy) {
        "definition"
      } else {
        "content"
      },
      asset = reference$asset %||% NA_character_,
      release_id = reference$release_id %||% NA_character_,
      run_id = reference$run_id %||% NA_character_
    )
    tables[[i]] <- data
  }
  primary <- tables[names(product$sources)]
  auxiliary <- lapply(names(product$transforms), function(step) {
    refs <- component_sources(product$transforms[[step]])
    stats::setNames(tables[transform_source_names(step, refs)], names(refs))
  })
  list(
    data = if (length(primary) == 1L) primary[[1]] else primary,
    transform_sources = stats::setNames(auxiliary, names(product$transforms)),
    inputs = dplyr::bind_rows(inputs),
    archives = archives
  )
}

# Classify the gate independently of backend-specific subclasses.
failure_subclass <- function(result) {
  if (identical(result$status, "blocked")) {
    checks <- result$quality
    contract <- checks$engine == "contract" &
      checks$status %in% c("failed", "error")
    return(
      if (any(contract, na.rm = TRUE)) {
        "dataraft_error_contract"
      } else {
        "dataraft_error_quality"
      }
    )
  }
  domains <- grep("^dataraft_error_", class(result$error), value = TRUE)
  if (length(domains)) domains else "dataraft_error_execution"
}
