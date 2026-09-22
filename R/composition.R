new_product <- function(
  id,
  contract,
  version,
  code_version,
  automatic_version,
  owner = NULL,
  description = NULL
) {
  rlang::local_error_call(rlang::caller_env())
  asset_id(id)
  scalar(version, "version")
  if (!is.null(owner)) {
    scalar(owner, "owner")
  }
  if (!is.null(description)) {
    scalar(description, "description")
  }
  if (!is.null(code_version)) {
    scalar(code_version, "code_version")
  }
  out <- structure(
    list(
      id = id,
      version = version,
      automatic_version = automatic_version,
      code_version = code_version,
      sources = list(),
      transforms = list(),
      contract = NULL,
      quality = list(),
      target = NULL,
      catalogs = list(),
      owner = owner,
      description = description
    ),
    class = "dr_product"
  )
  if (!is.null(contract)) {
    out <- dr_add_contract(out, contract)
  }
  out
}


editable_product <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (!inherits(x, "dr_product")) {
    abort(
      subclass = "dataraft_error_definition",
      "Start with dr_product('name') to compose a product with add_*()."
    )
  }
  attr(x, "dr_validated") <- NULL
  x
}


#' Bind a source to a product or workflow
#'
#' Store a named primary input without reading it. A workflow can instead bind
#' one delivery with `data =` at execution. For several primary sources, use a
#' transformation that combines their named list before table operations.
#' @param x A [dr_product()] or modular [dr_workflow()] definition.
#' @param source Data frame, file path, function or source adapter.
#' @param name Optional source name, generated when omitted.
#' @param reader Optional file reader. CSV, TSV, RDS and Excel have defaults.
#' @param replace Replace a source with the same name explicitly.
#' @returns An updated definition. No source data are read.
#' @seealso [dr_replace_sources()], [dataraft.adapters::dr_source_database()], [dr_trial()]
#' @export
#' @examples
#' flow <- dr_workflow() |>
#'   dr_add_product(dr_product("orders")) |>
#'   dr_add_source(data.frame(id = 1:2, amount = c(25, 75)), name = "orders")
#' dr_collect(dr_trial(flow))
dr_add_source <- function(
  x,
  source,
  name = NULL,
  reader = NULL,
  replace = FALSE
) {
  if (inherits(x, "dr_product_workflow")) {
    holder <- dr_product("workflow")
    holder$sources <- x$sources
    holder <- dr_add_source(holder, source, name, reader, replace)
    x$sources <- holder$sources
    check_workflow_slots(x)
    return(x)
  }
  x <- editable_product(x)
  flag(replace, "replace")
  if (is.null(name) && replace) {
    if (length(x$sources) != 1L) {
      abort(
        subclass = "dataraft_error_definition",
        "Name the source to replace when the product does not have exactly one primary source."
      )
    }
    name <- names(x$sources)[[1]]
  }
  name <- name %||%
    if (inherits(source, "dr_product")) {
      source$id
    } else {
      paste0("source_", length(x$sources) + 1L)
    }
  scalar(name, "name")
  if (name %in% names(x$sources) && !replace) {
    abort(
      subclass = "dataraft_error_definition",
      paste0(
        "Source `",
        name,
        "` already exists. Use replace = TRUE to replace it."
      )
    )
  }
  x$sources[[name]] <- normalize_source(source, x$id, name, reader)
  x
}


normalize_source <- function(source, id, name, reader = NULL) {
  rlang::local_error_call(rlang::caller_env())
  if (inherits(source, "dr_product_workflow")) {
    source <- compile_product_workflow(source)
  }
  if (!is.null(reader) && (!is.character(source) || !is.function(reader))) {
    abort(
      subclass = "dataraft_error_definition",
      "reader is only used with a file path and must be a function."
    )
  }
  if (inherits(source, "dr_run_result")) {
    source <- normalize_result_source(source)
  }
  if (is.character(source)) {
    scalar(source, "source path")
    if (
      is.null(reader) &&
        tolower(tools::file_ext(source)) %in% c("parquet", "pq")
    ) {
      source <- dataraft.adapters::dr_source_parquet(source)
    } else {
      source <- dr_source_file(
        paste0(
          id,
          ".source.",
          substr(digest::digest(name, algo = "sha256"), 1L, 12L)
        ),
        source,
        reader = reader %||% simple_reader(source)
      )
    }
  }
  if (!component_method("dr_read_source", source)) {
    abort(
      subclass = "dataraft_error_definition",
      "source must be a table, path, function, product, successful run or source adapter."
    )
  }
  source
}

#' Add a transformation directly to a product
#'
#' For modular workflows, prefer [dr_recipe()] with [dr_step_transform()]. This
#' direct interface appends preparation to a product for compact pipelines.
#' @param x A table product definition.
#' @param transform R function, formula using `.x`, or transform adapter.
#' @param name Optional unique step name, generated when omitted.
#' @returns An updated product definition. Execution remains deferred.

#' @examples
#' dr_product("orders", data.frame(amount = c(10, 20))) |>
#'   dr_add_recipe(dr_recipe() |> dr_step_transform(~ dplyr::mutate(.x, amount = amount * 2))) |>
#'   dr_trial() |>
#'   dr_collect()
#' @keywords internal
#' @noRd
dr_add_transform <- function(x, transform, name = NULL) {
  if (inherits(x, "dr_model_product")) {
    abort(
      subclass = "dataraft_error_definition",
      "Transform a member table product, then use dr_replace_sources(model, table_name = product)."
    )
  }
  x <- editable_product(x)
  if (inherits(transform, "formula")) {
    transform <- rlang::as_function(transform)
  }
  if (!component_method("dr_execute_transform", transform)) {
    abort(
      subclass = "dataraft_error_definition",
      "transform must be a function, formula using .x, or transform adapter."
    )
  }
  name <- name %||% paste0("transform_", length(x$transforms) + 1L)
  scalar(name, "name")
  if (name %in% names(x$transforms)) {
    abort(
      subclass = "dataraft_error_definition",
      "Transformation names must be unique."
    )
  }
  x$transforms[[name]] <- transform
  x
}

#' Attach output requirements to a product specification
#'
#' The contract describes shape and keys; quality rules describe acceptable
#' values. Checks apply after preparation and gate framework writers.
#' Adding a contract replaces the previous contract. Quality rules accumulate.
#' @param x A [dr_product()] specification or modular [dr_workflow()].
#' @param contract Contract, named type vector, or named list of prototypes.
#' @param quality One-sided row predicate, function, rule, or list of rules.
#'   Formula `NA` results count as failures. Functions return scalar logicals
#'   or [dr_quality_counts()]. Names in rule lists become rule names.
#' @param name Optional quality rule name.
#' @returns An updated product specification, without executing checks.
#' @seealso [dr_contract()], [dr_quality_rule()], [dr_set_engine()]
#' @export
#' @examples
#' dr_product("orders") |>
#'   dr_add_contract(c(id = "integer", amount = "numeric")) |>
#'   dr_add_quality(~ amount >= 0)
dr_add_contract <- function(x, contract) {
  if (inherits(x, "dr_product_workflow")) {
    x$product <- dr_add_contract(dr_extract_product(x), contract)
    check_workflow_slots(x)
    return(x)
  }
  x <- editable_product(x)
  if (!inherits(contract, "dr_contract")) {
    contract <- dr_contract(paste0(x$id, ".contract"), columns = contract)
  }
  if (isTRUE(attr(contract, "dr_anonymous"))) {
    contract$id <- paste0(x$id, ".contract")
    attr(contract, "dr_anonymous") <- NULL
  }
  assert_contract_ready(contract)
  x$contract <- contract
  x
}

#' @rdname dr_add_contract
#' @inheritParams dr_quality_rule
#' @param engine Optional formula quality engine, `"native"` or
#'   `"pointblank"`. Omit to preserve engines on existing rule specifications.
#' @export
dr_add_quality <- function(
  x,
  quality,
  name = NULL,
  engine = NULL,
  action = NULL,
  threshold = NULL,
  dimension = NULL
) {
  old_count <- length(x$quality)
  x <- editable_product(x)
  x$quality <- normalize_quality_rules(
    quality,
    name,
    engine,
    existing = x$quality
  )
  if (length(x$quality) > old_count) {
    for (i in seq.int(old_count + 1L, length(x$quality))) {
      rule <- x$quality[[i]]
      if (!is.null(action)) {
        rule$action <- match.arg(action, c("block", "warn", "quarantine"))
        rule$severity <- if (action == "warn") "warning" else "error"
      }
      if (!is.null(threshold)) {
        if (
          !is.numeric(threshold) ||
            length(threshold) != 1L ||
            !is.finite(threshold) ||
            threshold < 0 ||
            threshold > 1
        ) {
          abort("threshold must be between zero and one.")
        }
        rule$max_failure <- threshold
      }
      if (!is.null(dimension)) {
        rule$dimension <- match.arg(
          dimension,
          c(
            "accuracy",
            "completeness",
            "conformity",
            "consistency",
            "coverage",
            "timeliness",
            "uniqueness"
          )
        )
      }
      x$quality[[i]] <- rule
    }
  }
  x
}


#' Set a publication destination
#'
#' Store or replace a destination without writing data. [dr_trial()] disables
#' it; [dr_run()] and [dr_publish()] execute it after successful output checks.
#' @param x A [dr_product()] or modular [dr_workflow()] definition.
#' @param target Lake folder path, connected lake, configuration or target
#'   adapter. Use [dataraft.lake::dr_target_lake()] for partition or layer options.
#' @returns An updated definition.
#' @export
#' @examplesIf requireNamespace("dataraft.lake", quietly = TRUE)
#' dr_workflow() |>
#'   dr_add_product(dr_product("orders")) |>
#'   dr_set_target("data/orders")
dr_set_target <- function(x, target) {
  if (inherits(x, "dr_product_workflow")) {
    x$target <- normalize_target(target)
    check_workflow_slots(x)
    return(x)
  }
  x <- editable_product(x)
  x$target <- normalize_target(target)
  x
}

#' Attach a metadata destination to a product
#'
#' Catalog callbacks receive descriptive run metadata after execution. Delivery
#' is outside the data transaction; retryable failures retain run evidence.
#' @param x A [dr_product()] definition.
#' @param catalog Function receiving run metadata, or catalog adapter.
#' @param name Optional unique catalog name, generated when omitted.
#' @returns An updated product definition.
#' @seealso [dataraft.catalog::dr_catalog_openlineage()], [dr_retry_catalogs()]
#' @export
#' @examples
#' dr_product("orders") |>
#'   dr_add_catalog(function(metadata) invisible(metadata), name = "audit")
dr_add_catalog <- function(x, catalog, name = NULL) {
  x <- editable_product(x)
  if (!component_method("dr_publish_metadata", catalog)) {
    abort(
      subclass = "dataraft_error_definition",
      "catalog must be a function or an adapter with dr_publish_metadata()."
    )
  }
  name <- name %||%
    if (is.function(catalog)) {
      paste0("callback-", length(x$catalogs) + 1L)
    } else {
      catalog$id %||% paste0("catalog-", length(x$catalogs) + 1L)
    }
  scalar(name, "name")
  if (name %in% names(x$catalogs)) {
    abort(
      subclass = "dataraft_error_definition",
      "Catalog names must be unique."
    )
  }
  x$catalogs[[name]] <- catalog
  x
}


#' @export
dr_validate.dr_product <- function(data, contract = NULL, ...) {
  rlang::check_dots_empty()
  if (!is.null(contract)) {
    abort(
      subclass = "dataraft_error_definition",
      "Add a contract with dr_add_contract() before preflight."
    )
  }
  validate_product_graph(data)
  attr(data, "dr_validated") <- TRUE
  data
}


validate_product_graph <- function(product) {
  rlang::local_error_call(rlang::caller_env())
  seen <- new.env(parent = emptyenv())
  visit <- function(data, stack = character()) {
    rlang::local_error_call(rlang::caller_env())
    asset_id(data$id)
    scalar(data$version, "version")
    if (data$id %in% stack) {
      abort(
        subclass = "dataraft_error_definition",
        paste0(
          "Product dependency cycle: ",
          paste(c(stack, data$id), collapse = " -> "),
          "."
        ),
        "dr_dependency_cycle"
      )
    }
    if (exists(data$id, seen, inherits = FALSE)) {
      previous <- get(data$id, seen, inherits = FALSE)
      attr(previous, "dr_validated") <- NULL
      current <- data
      attr(current, "dr_validated") <- NULL
      if (!identical(previous, current)) {
        abort(
          subclass = "dataraft_error_definition",
          paste0(
            "Different definitions use product id `",
            data$id,
            "`. Give each product a unique id."
          ),
          "dr_dependency_conflict"
        )
      }
      return(invisible(NULL))
    }
    if (!length(data$sources)) {
      abort(
        subclass = "dataraft_error_definition",
        "This product has no source. Add one with dr_add_source()."
      )
    }
    if (
      is.null(names(data$sources)) ||
        anyDuplicated(names(data$sources)) ||
        anyNA(names(data$sources)) ||
        any(!nzchar(names(data$sources)))
    ) {
      abort(
        subclass = "dataraft_error_definition",
        "Product sources must have unique, non-empty names."
      )
    }
    for (source in product_sources(data)) {
      if (inherits(source, "dr_product")) {
        visit(source, c(stack, data$id))
      } else {
        assert_component(source, "dr_read_source")
      }
    }
    for (step in data$transforms) {
      assert_component(step, "dr_execute_transform")
    }
    if (!is.null(data$contract)) {
      assert_contract_ready(data$contract)
      args <- data$contract
      args[c("kind", "automatic_schema")] <- NULL
      args$columns <- unlist(args$columns, use.names = TRUE)
      do.call(dr_contract, args)
    }
    rules <- c(data$contract$rules, data$quality)
    if (anyDuplicated(vapply(rules, `[[`, character(1), "name"))) {
      abort(
        subclass = "dataraft_error_definition",
        "Contract and added quality rules must have unique names."
      )
    }
    for (rule in rules) {
      assert_component(rule, "dr_run_quality")
    }
    if (!is.null(data$target)) {
      dr_check_component(data$target)
      if (
        !component_method("dr_execute_target", data$target) &&
          !component_method("dr_write_target", data$target)
      ) {
        abort(
          subclass = "dataraft_error_definition",
          "The target needs a dr_write_target() method."
        )
      }
    }
    normalize_catalogs(data$catalogs)
    for (catalog in data$catalogs) {
      assert_component(catalog, "dr_publish_metadata")
    }
    assign(data$id, data, seen)
    invisible(NULL)
  }
  visit(product)
  invisible(product)
}


#' Inspect a product or run without executing it
#'
#' Product inspection includes identity, source description, ordered steps,
#' contract, target and optional integrations. Data rows, connection credentials
#' and closure environments are omitted. This is descriptive metadata, not a
#' portable executable serialization. Save project R code for reproducibility.
#' Extension packages may implement `dr_inspect()` to provide safe descriptors.
#' @param x Product, run or component.
#' @param ... Reserved for extensions.
#' @returns A descriptive list of the component or execution result.
#' @export
#' @examples
#' orders <- dr_product("orders") |> dr_add_source(data.frame(id = 1:2))
#' dr_inspect(orders)
#' dr_plan(orders)
dr_inspect <- function(x, ...) UseMethod("dr_inspect")

#' @export
dr_inspect.default <- function(x, ...) list(type = class(x)[[1]])

#' @export
dr_inspect.NULL <- function(x, ...) list(type = "memory")

#' @export
dr_inspect.data.frame <- function(x, ...) {
  list(type = "data.frame", rows = nrow(x), columns = names(x))
}

#' @export
dr_inspect.function <- function(x, ...) {
  list(type = "R function", code = canonical(x))
}

# Runtime connection factories can be described without claiming their
# external state has a reproducible semantic fingerprint. Transform and rule
# inspection remains strict; this fallback applies only in source positions.
inspect_source <- function(source) {
  tryCatch(dr_inspect(source), dataraft_error_fingerprint = function(e) {
    if (!is.function(source)) {
      stop(e)
    }
    list(
      type = "R function",
      code = list(
        formals = paste(deparse(formals(source)), collapse = "\n"),
        body = paste(deparse(body(source)), collapse = "\n")
      ),
      fingerprintable = FALSE,
      dynamic = TRUE
    )
  })
}

#' @export
dr_inspect.dr_source <- function(x, ...) {
  list(type = "file", id = x$id, path = x$path, reader = canonical(x$reader))
}

#' @export
dr_inspect.dr_product <- function(x, ...) {
  sources <- lapply(x$sources, function(source) {
    if (inherits(source, "dr_product")) {
      list(type = "product", id = source$id, version = source$version)
    } else {
      inspect_source(source)
    }
  })
  list(
    id = x$id,
    version = x$version,
    code_version = x$code_version,
    status = if (isTRUE(attr(x, "dr_validated"))) "validated" else "defined",
    sources = sources,
    transforms = lapply(x$transforms, dr_inspect),
    contract = canonical(effective_product_contract(x)),
    quality = canonical(x$quality),
    target = dr_inspect(x$target),
    catalogs = lapply(x$catalogs, dr_inspect),
    owner = x$owner %||% x$contract$owner %||% "",
    description = x$description %||% x$contract$description %||% "",
    plan = product_plan(x, check = FALSE)
  )
}

#' @export
dr_inspect.dr_run_result <- function(x, ...) {
  x[c(
    "run_id",
    "status",
    "release_id",
    "asset",
    "started_at",
    "finished_at",
    "backend",
    "inputs",
    "outputs",
    "quality",
    "warnings",
    "metadata",
    "lifecycle"
  )]
}

#' @rdname dr_inspect
#' @importFrom dplyr explain

#' @keywords internal
dr_explain <- function(x, ...) dplyr::explain(x, ...)


#' @rdname dr_inspect
#' @export
explain.dr_product <- function(x, ...) {
  rlang::local_error_call(rlang::caller_env())
  rlang::check_dots_empty()
  target <- product_display_target(x)
  text <- c(
    paste0("Product: ", x$id),
    paste0("Read: ", length(x$sources), " named source(s)."),
    paste0(
      "Deliveries: ",
      paste(names(delivery_aliases(x)), collapse = ", "),
      "."
    ),
    "Replace a delivery with sources = list(delivery_name = new_data).",
    if (length(product_sources(x)) > length(x$sources)) {
      paste0(
        "Lookups: ",
        length(product_sources(x)) - length(x$sources),
        " auxiliary source(s), acquired once before transformations."
      )
    },
    if (length(x$sources) > 1L) {
      "The first transform receives a named list; combine it into one table."
    } else {
      "Transforms receive one table, which may stay lazy."
    },
    paste0("Transform: ", length(x$transforms), " ordered step(s)."),
    paste0(
      "Check: ",
      if (is.null(x$contract)) {
        "inferred structure"
      } else {
        "declared contract"
      },
      " and ",
      length(x$quality),
      " additional rule(s)."
    ),
    if (is.null(target)) {
      "Return: checked data and run evidence. dr_collect() materializes lazy output."
    } else {
      paste0(
        "Publish: ",
        dr_inspect(target)$type,
        ". Failed checks block publication."
      )
    },
    if (identical(dr_capabilities(target)$lazy, FALSE)) {
      "Materialization: the target requires an ordinary table."
    } else {
      "Materialization: lazy tables remain lazy unless a component collects."
    },
    product_display_defaults(x),
    "dr_validate() checks configuration and dependency cycles; dr_run() executes."
  )
  cat(paste(text, collapse = "\n"), "\n", sep = "")
  invisible(text)
}

#' @export
print.dr_product <- function(x, ...) {
  cat("<Data product:", x$id, ">\n")
  cat(
    "Deliveries: ",
    if (!length(delivery_aliases(x))) {
      "not set"
    } else {
      paste(names(delivery_aliases(x)), collapse = ", ")
    },
    "\n",
    sep = ""
  )
  cat("Transformations: ", length(x$transforms), "\n", sep = "")
  cat(
    "Contract: ",
    if (is.null(x$contract)) {
      "automatic structure"
    } else {
      paste(length(x$contract$columns), "fields, version", x$contract$version)
    },
    "\n",
    sep = ""
  )
  cat("Quality:", length(x$quality) + length(x$contract$rules), "rules\n")
  target <- product_display_target(x)
  target_label <- dr_inspect(target)$type
  if (inherits(target, "dr_lake_target")) {
    target_label <- paste0(target_label, " (", target$layer, ")")
  }
  cat("Target: ", target_label, "\n", sep = "")
  defaults <- product_display_defaults(x)
  if (!is.null(defaults)) {
    cat(defaults, "\n")
  }
  cat(
    "Status: ",
    if (isTRUE(attr(x, "dr_validated"))) {
      "validated"
    } else {
      "defined"
    },
    "\n",
    sep = ""
  )
  invisible(x)
}


product_display_target <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  execution <- attr(x, "dr_execution_config", exact = TRUE)
  target <- x$target %||% execution$to
  if (
    inherits(target, "dr_lake_target") &&
      !is.null(execution$layer) &&
      (is.null(x$target) || is.null(target$layer))
  ) {
    target$layer <- execution$layer
  }
  target
}


product_display_defaults <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  execution <- attr(x, "dr_execution_config", exact = TRUE)
  if (is.null(execution)) {
    return(NULL)
  }
  paste0(
    "Stored execution (root only): quality = ",
    execution$quality,
    "; relationships = ",
    execution$relationships,
    ". Override with dr_run(execution = )."
  )
}


product_plan <- function(x, check = TRUE) {
  rlang::local_error_call(rlang::caller_env())
  sources <- product_sources(x)
  source_types <- vapply(
    sources,
    function(source) {
      if (inherits(source, "dr_product")) {
        "product"
      } else {
        inspect_source(source)$type
      }
    },
    character(1)
  )
  steps <- c(
    rep("read", length(sources)),
    rep("transform", length(x$transforms)),
    "validate",
    "publish",
    rep("catalog", length(x$catalogs))
  )
  ids <- c(
    names(sources),
    names(x$transforms),
    x$contract$id %||% "automatic structure",
    x$id,
    if (length(x$catalogs)) names(normalize_catalogs(x$catalogs))
  )
  components <- c(sources, x$transforms, list(NULL, x$target), x$catalogs)
  lazy <- vapply(
    components,
    function(component) dr_capabilities(component)$lazy,
    logical(1)
  )
  out <- tibble::tibble(
    position = seq_along(steps),
    step = steps,
    id = ids,
    target = c(
      source_types,
      vapply(x$transforms, function(step) dr_inspect(step)$type, character(1)),
      "contract and quality",
      dr_inspect(x$target)$type,
      rep("metadata only", length(x$catalogs))
    ),
    materializes = ifelse(is.na(lazy), NA, !lazy)
  )
  if (check) {
    attr(out, "complete") <- tryCatch(
      {
        dr_validate(x)
        TRUE
      },
      error = function(e) FALSE
    )
  }
  out
}
