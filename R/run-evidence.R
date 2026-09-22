#' Read durable execution history and quality incidents
#'
#' Pass `evidence = "runs"` to [dr_run()] to keep one JSON record per run.
#' Evidence contains descriptive metadata and quality counts, never input rows,
#' executable definitions, connections, request objects or raw conditions.
#' URL credentials and query strings are removed. Business descriptions and
#' identifiers remain visible: treat the directory as operational metadata.
#'
#' Records are written by replacing a file in the same directory. Use a single
#' coordinated writer for a directory, including [dr_retry_catalogs()]. This is
#' a local evidence store, not a distributed transaction log. Writing data and
#' recording evidence are separate operations; evidence errors never imply that
#' published data was rolled back. A successful write followed by a process
#' crash before recording evidence remains a reconciliation responsibility.
#' @param path Evidence directory.
#' @param product Optional product name to filter.
#' @param run_id Run identifier returned by [dr_run()].
#' @param x A run result, an evidence record or an evidence directory.
#' @returns `dr_run_history()` and `dr_incidents()` return tibbles. `dr_read_run()`
#'   returns a descriptive list, including catalog delivery state.
#' @export
#' @examples
#' path <- tempfile("runs-")
#' result <- dr_product("orders") |>
#'   dr_add_source(data.frame(id = 1:2)) |>
#'   dr_run(evidence = path)
#' dr_run_history(path)
#' dr_read_run(path, result$run_id)$status
#' dr_incidents(path)
#' unlink(path, recursive = TRUE)
dr_run_history <- function(path, product = NULL) {
  if (!is.null(product)) {
    scalar(product, "product")
  }
  records <- evidence_records(path)
  rows <- lapply(records, function(x) {
    tibble::tibble(
      run_id = x$run_id,
      product = x$product,
      status = x$status,
      started_at = x$started_at %||% NA_character_,
      finished_at = x$finished_at %||% NA_character_,
      backend = x$backend %||% NA_character_,
      rows = x$rows %||% NA_real_,
      pending_catalogs = sum(vapply(
        x$deliveries,
        function(d) {
          identical(d$status, "pending")
        },
        logical(1)
      ))
    )
  })
  out <- dplyr::bind_rows(c(
    list(tibble::tibble(
      run_id = character(),
      product = character(),
      status = character(),
      started_at = character(),
      finished_at = character(),
      backend = character(),
      rows = numeric(),
      pending_catalogs = integer()
    )),
    rows
  ))
  if (!is.null(product)) {
    out <- out[out$product == product, , drop = FALSE]
  }
  out[order(out$started_at, out$run_id), , drop = FALSE]
}


#' @rdname dr_run_history
#' @export
dr_read_run <- function(path, run_id) {
  file <- evidence_file(path, run_id)
  if (!file.exists(file)) {
    abort(
      subclass = "dataraft_error_definition",
      "This run has no saved evidence in path."
    )
  }
  read_evidence_file(file)
}


#' @rdname dr_run_history
#' @export
dr_incidents <- function(x) {
  records <- if (inherits(x, "dr_run_result")) {
    list(safe_run_evidence(x))
  } else if (is.character(x) && length(x) == 1L) {
    evidence_records(x)
  } else if (is.list(x) && !is.null(x$run_id)) {
    list(x)
  } else {
    abort(
      subclass = "dataraft_error_definition",
      "Use a run result, saved run record or evidence directory."
    )
  }
  empty <- tibble::tibble(
    run_id = character(),
    product = character(),
    rule = character(),
    status = character(),
    stage = character(),
    engine = character(),
    n_failed = numeric(),
    n_total = numeric()
  )
  rows <- lapply(records, function(record) {
    checks <- record$quality
    if (!length(checks)) {
      return(empty)
    }
    checks <- dplyr::bind_rows(checks)
    checks <- checks[
      checks$status %in%
        c("failed", "fail", "error", "not_checked"),
      ,
      drop = FALSE
    ]
    if (!nrow(checks)) {
      return(empty)
    }
    value <- function(name, default) {
      rlang::local_error_call(rlang::caller_env())
      checks[[name]] %||% rep(default, nrow(checks))
    }
    tibble::tibble(
      run_id = record$run_id,
      product = record$product,
      rule = value("rule", NA_character_),
      status = checks$status,
      stage = value("stage", NA_character_),
      engine = value("engine", NA_character_),
      n_failed = value("n_failed", NA_real_),
      n_total = value("n_total", NA_real_)
    )
  })
  dplyr::bind_rows(c(list(empty), rows))
}


#' Retry pending catalog deliveries using fresh credentials
#'
#' Reuses the saved run ID, timestamps and sanitized metadata. Only pending
#' deliveries to matching destination IDs are attempted. Supply the adapters
#' again, with fresh authentication if needed. Request objects and callbacks
#' are never serialized. Named callback lists must use the same names passed
#' to `dr_add_catalog(name = ...)`; unnamed callbacks use `callback-1`, etc.
#'
#' Delivery is at least once: a process can stop after the remote server
#' accepts a request but before local acknowledgement is saved. Consumers
#' should deduplicate by run/event identity. Successful recorded deliveries
#' are not resent. Use one coordinated writer for this directory.
#' @param path Evidence directory.
#' @param catalogs A catalog adapter, callback, or list of catalog destinations.
#' @returns Updated run history, invisibly. Delivery failures remain pending.
#' @export
#' @examples
#' # catalog <- dataraft.catalog::dr_catalog_openlineage(
#' #   "https://lineage.example/api/v1/lineage")
#' # dr_retry_catalogs("runs", list(catalog))
dr_retry_catalogs <- function(path, catalogs) {
  catalogs <- normalize_catalogs(catalogs)
  for (record in evidence_records(path)) {
    for (id in intersect(names(record$deliveries), names(catalogs))) {
      if (!identical(record$deliveries[[id]]$status, "pending")) {
        next
      }
      record <- deliver_catalog(record, catalogs[[id]], id)
      save_run_evidence(record, path)
    }
  }
  invisible(dr_run_history(path))
}


finalize_product_run <- function(result, product, evidence = NULL) {
  rlang::local_error_call(rlang::caller_env())
  record <- safe_run_evidence(result, product)
  catalogs <- normalize_catalogs(product$catalogs)
  for (id in names(catalogs)) {
    catalog <- catalogs[[id]]
    supported <- if (inherits(catalog, "dr_openlineage_catalog")) {
      TRUE
    } else {
      record$status %in% c("completed", "published", "cached")
    }
    if (supported) {
      record$deliveries[[id]] <- list(
        status = "pending",
        attempts = 0L,
        updated_at = record$finished_at
      )
    }
  }
  persist <- function(record) {
    rlang::local_error_call(rlang::caller_env())
    if (is.null(evidence)) {
      return(TRUE)
    }
    tryCatch(
      {
        save_run_evidence(record, evidence)
        TRUE
      },
      error = function(e) {
        result$warnings <<- c(
          result$warnings,
          "Run evidence could not be saved; the execution status is unchanged. Inspect result$evidence_error locally."
        )
        result$evidence_error <<- e
        FALSE
      }
    )
  }
  saved <- persist(record)
  for (id in names(record$deliveries)) {
    record <- deliver_catalog(record, catalogs[[id]], id)
    if (identical(record$deliveries[[id]]$status, "pending")) {
      result$warnings <- c(
        result$warnings,
        paste0(
          "Catalog `",
          id,
          "` delivery failed; execution status is unchanged. ",
          if (saved && !is.null(evidence)) {
            "Retry with dr_retry_catalogs()."
          } else {
            "Inspect result$catalog_delivery and retry with fresh metadata."
          }
        )
      )
    }
    persist(record)
  }
  result$catalog_delivery <- record$deliveries
  result["evidence"] <- list(
    if (saved && !is.null(evidence)) {
      evidence_file(evidence, result$run_id)
    } else {
      NULL
    }
  )
  result
}


normalize_catalogs <- function(catalogs) {
  rlang::local_error_call(rlang::caller_env())
  if (!length(catalogs)) {
    return(list())
  }
  if (is.function(catalogs) || inherits(catalogs, "dr_catalog_adapter")) {
    catalogs <- list(catalogs)
  }
  if (!is.list(catalogs)) {
    abort(
      subclass = "dataraft_error_definition",
      "catalogs must be catalog adapters or callbacks."
    )
  }
  ids <- vapply(
    seq_along(catalogs),
    function(i) {
      catalog <- catalogs[[i]]
      named <- names(catalogs)[i]
      if (length(named) && !is.na(named) && nzchar(named)) {
        return(named)
      }
      if (is.list(catalog) && !is.null(catalog$id)) {
        return(catalog$id)
      }
      paste0("callback-", i)
    },
    character(1)
  )
  if (anyDuplicated(ids)) {
    abort(
      subclass = "dataraft_error_definition",
      "Catalog destination names must be unique."
    )
  }
  stats::setNames(catalogs, ids)
}


deliver_catalog <- function(record, catalog, id) {
  rlang::local_error_call(rlang::caller_env())
  delivery <- record$deliveries[[id]]
  delivery$attempts <- delivery$attempts + 1L
  delivery$updated_at <- now()
  delivery$error_class <- NULL
  tryCatch(
    {
      dr_publish_metadata(catalog, record)
      delivery$status <- "delivered"
    },
    error = function(e) {
      delivery$status <<- "pending"
      delivery$error_class <<- class(e)[[1L]]
    }
  )
  record$deliveries[[id]] <- delivery
  record
}


safe_run_evidence <- function(result, product = NULL) {
  rlang::local_error_call(rlang::caller_env())
  metadata <- result$metadata %||% list()
  contract <- metadata$contract %||% product$contract %||% list()
  definition <- metadata$definition %||% list()
  checks <- result$quality %||% metadata$quality
  if (is.data.frame(checks)) {
    checks <- lapply(seq_len(nrow(checks)), function(i) {
      as.list(checks[i, , drop = FALSE])
    })
  }
  check_fields <- c(
    "rule",
    "status",
    "stage",
    "engine",
    "severity",
    "n_failed",
    "n_total",
    "threshold",
    "failure_rate"
  )
  record <- list(
    format_version = 1L,
    run_id = result$run_id,
    product = result$asset %||% metadata$product %||% product$id,
    status = result$status,
    started_at = safe_scalar(result$started_at %||% metadata$started_at),
    finished_at = safe_scalar(result$finished_at %||% metadata$finished_at),
    backend = safe_scalar(result$backend %||% metadata$backend),
    rows = safe_scalar(metadata$rows, "numeric"),
    schema = safe_schema(metadata$schema),
    code_version = safe_scalar(metadata$code_version %||% product$code_version),
    definition = safe_fields(
      definition,
      c("id", "version", "owner", "description")
    ),
    contract = safe_fields(
      contract,
      c(
        "id",
        "version",
        "owner",
        "description",
        "grain",
        "columns",
        "required",
        "key"
      )
    ),
    quality = unname(lapply(checks, safe_fields, fields = check_fields)),
    inputs = safe_descriptors(result$inputs %||% metadata$inputs),
    outputs = safe_descriptor(result$outputs %||% metadata$outputs),
    release_id = safe_scalar(result$release_id),
    deliveries = list()
  )
  governance <- contract$governance %||% list()
  public_governance <- safe_fields(
    governance,
    c("steward", "classification", "pii", "retention")
  )
  for (field in c("tags", "glossary")) {
    value <- unlist(governance[[field]], recursive = FALSE, use.names = FALSE)
    if (is.character(value) && !is.object(value)) {
      public_governance[[field]] <- unname(as.list(safe_text(value)))
    }
  }
  if (is.list(governance$openmetadata_owners)) {
    public_governance$openmetadata_owners <- unname(lapply(
      governance$openmetadata_owners,
      safe_fields,
      fields = c("id", "type")
    ))
  }
  if (length(public_governance)) {
    record$contract$governance <- public_governance
  }
  if (is.list(contract$column_metadata)) {
    record$contract$column_metadata <- lapply(
      contract$column_metadata,
      safe_fields,
      fields = c("description", "classification", "businessName")
    )
  }
  lineage <- metadata$column_lineage
  if (
    is.list(lineage) &&
      isTRUE(lineage$complete) &&
      is.list(lineage$fields) &&
      !is.null(names(lineage$fields)) &&
      !anyDuplicated(names(lineage$fields)) &&
      all(vapply(
        lineage$fields,
        function(x) is.character(x) && !is.object(x) && !anyNA(x),
        logical(1)
      ))
  ) {
    record$column_lineage <- list(
      complete = TRUE,
      fields = lapply(lineage$fields, safe_text)
    )
  }
  if (!is.null(result$error)) {
    record$error <- list(
      class = class(result$error)[[1L]],
      stage = safe_scalar(result$error$stage) %||% "execution",
      message = "Execution failed; inspect the original condition locally."
    )
  }
  if (length(contract$columns)) {
    record$contract$columns <- safe_schema(contract$columns)
  }
  record
}


safe_text <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.character(x)) {
    return(x)
  }
  url <- grepl("^[A-Za-z][A-Za-z0-9+.-]*://", x)
  url[is.na(url)] <- FALSE
  x[url] <- sub("[?#].*$", "", x[url])
  x[url] <- sub("(^[A-Za-z][A-Za-z0-9+.-]*://)[^/@]*@", "\\1", x[url])
  x
}


safe_fields <- function(x, fields) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.list(x)) {
    return(list())
  }
  out <- x[intersect(names(x), fields)]
  for (name in names(out)) {
    value <- out[[name]]
    vector <- name %in% c("columns", "required", "key")
    if (
      is.atomic(value) && !is.object(value) && (vector || length(value) == 1L)
    ) {
      out[name] <- list(safe_text(value))
    } else {
      out[name] <- list(NULL)
    }
  }
  out
}


safe_scalar <- function(x, type = "character") {
  rlang::local_error_call(rlang::caller_env())
  valid <- if (type == "numeric") is.numeric(x) else is.character(x)
  if (valid && !is.object(x) && length(x) == 1L) safe_text(x) else NULL
}


safe_schema <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (is.atomic(x) && !is.null(names(x))) {
    return(as.list(safe_text(x)))
  }
  if (is.list(x)) {
    return(lapply(x, function(value) {
      if (is.character(value) && length(value) == 1L) value else NULL
    }))
  }
  NULL
}


safe_descriptor <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  fields <- c(
    "type",
    "id",
    "name",
    "path",
    "table",
    "schema",
    "namespace",
    "rows",
    "hash",
    "fingerprint",
    "hash_kind",
    "backend",
    "version",
    "release_id",
    "snapshot",
    "run_id",
    "product",
    "asset"
  )
  out <- safe_fields(x, fields)
  if (is.list(x) && is.list(x$dataset)) {
    out$dataset <- safe_fields(x$dataset, c("namespace", "name"))
  }
  if (is.list(x) && is.list(x$source)) {
    out$source <- safe_descriptor(x$source)
  } else if (is.list(x) && is.character(x$source) && length(x$source) == 1L) {
    out$source <- safe_fields(
      list(
        type = "lake input",
        id = x$source,
        version = x$source_version,
        path = x$landed_path %||% x$original_name
      ),
      c("type", "id", "version", "path")
    )
  }
  out
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name safe_descriptors

safe_descriptors <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      descriptor <- as.list(x[i, , drop = FALSE])
      if (is.list(descriptor$source) && length(descriptor$source) == 1L) {
        descriptor$source <- descriptor$source[[1L]]
      }
      safe_descriptor(descriptor)
    }))
  }
  if (!is.list(x)) {
    return(list())
  }
  if (any(c("source", "type", "id", "hash") %in% names(x))) {
    return(list(safe_descriptor(x)))
  }
  lapply(x, safe_descriptor)
}


evidence_file <- function(path, run_id) {
  rlang::local_error_call(rlang::caller_env())
  scalar(path, "path")
  scalar(run_id, "run_id")
  if (!grepl("^[A-Za-z0-9_-]+$", run_id)) {
    abort(subclass = "dataraft_error_definition", "Invalid run identifier.")
  }
  file.path(path, paste0(run_id, ".json"))
}


save_run_evidence <- function(record, path) {
  rlang::local_error_call(rlang::caller_env())
  file <- evidence_file(path, record$run_id)
  if (
    !dir.exists(path) &&
      !dir.create(path, recursive = TRUE, showWarnings = FALSE)
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Cannot create the run evidence directory."
    )
  }
  temporary <- tempfile(".run-", tmpdir = path)
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    record,
    temporary,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = TRUE,
    digits = NA
  )
  if (!suppressWarnings(file.rename(temporary, file))) {
    abort(
      subclass = "dataraft_error_definition",
      "Cannot replace the run evidence file."
    )
  }
  invisible(file)
}


read_evidence_file <- function(file) {
  rlang::local_error_call(rlang::caller_env())
  record <- tryCatch(
    jsonlite::read_json(file, simplifyVector = FALSE),
    error = function(e) {
      abort(
        subclass = "dataraft_error_definition",
        paste0("Cannot read run evidence: ", basename(file))
      )
    }
  )
  if (
    !identical(record$format_version, 1L) ||
      is.null(record$run_id) ||
      is.null(record$product) ||
      is.null(record$status)
  ) {
    abort(
      subclass = "dataraft_error_definition",
      paste0("Unsupported or malformed run evidence: ", basename(file))
    )
  }
  record
}


evidence_records <- function(path) {
  rlang::local_error_call(rlang::caller_env())
  scalar(path, "path")
  if (!dir.exists(path)) {
    abort(
      subclass = "dataraft_error_definition",
      "The run evidence directory does not exist."
    )
  }
  lapply(
    list.files(path, pattern = "^[A-Za-z0-9_-]+\\.json$", full.names = TRUE),
    read_evidence_file
  )
}
