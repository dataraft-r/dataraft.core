#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name abort

abort <- function(
  message,
  class = NULL,
  ...,
  subclass = NULL,
  call = rlang::caller_env()
) {
  result <- list(...)$result
  if (inherits(result, c("dr_run_result", "dr_model_result"))) {
    .last_failure$result <- result
  }
  rlang::abort(
    message,
    class = unique(c(class, subclass, "dataraft_error")),
    ...,
    call = call
  )
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name need

need <- function(package, purpose = NULL) {
  if (!requireNamespace(package, quietly = TRUE)) {
    purpose <- purpose %||%
      switch(
        package,
        duckdb = "Local lake storage",
        dm = "Relational table models",
        pointblank = "The pointblank quality engine",
        pins = "Pin-based storage",
        "This optional integration"
      )
    install <- if (startsWith(package, "dataraft.")) {
      sprintf('pak::pak("dataraft-r/%s")', package)
    } else {
      sprintf('install.packages("%s")', package)
    }
    message <- c(
      "{purpose} requires the optional package {.pkg {package}}.",
      i = "Install it with {.code {install}}."
    )
    if (identical(package, "duckdb")) {
      message <- c(
        message,
        i = "For an in-memory workflow, use {.fn dr_trial} instead of lake publication."
      )
    }
    cli::cli_abort(
      message,
      class = c(
        "dataraft_error_dependency",
        "dataraft_error_definition",
        "dataraft_error"
      ),
      package = package,
      purpose = purpose,
      call = rlang::caller_env()
    )
  }
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name scalar

scalar <- function(x, what) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    abort(
      subclass = "dataraft_error_definition",
      paste(what, "must be a non-empty string.")
    )
  }
  x
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name absolute_path

absolute_path <- function(path) {
  rlang::local_error_call(rlang::caller_env())
  path <- path.expand(scalar(path, "path"))
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)) {
    path <- file.path(getwd(), path)
  }
  # Canonicalize the existing ancestor before appending missing components.
  # Normalizing a nonexistent path directly can retain Windows short names or
  # separators that change after creation, invalidating an unchanged targets DAG.
  suffix <- character()
  while (!file.exists(path)) {
    parent <- dirname(path)
    if (identical(parent, path)) {
      break
    }
    suffix <- c(basename(path), suffix)
    path <- parent
  }
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  for (component in suffix) {
    path <- switch(
      component,
      "." = path,
      ".." = dirname(path),
      file.path(path, component)
    )
  }
  path
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name ident

ident <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  scalar(x, "Identifier")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", x)) {
    abort(
      subclass = "dataraft_error_definition",
      paste("Invalid identifier:", x)
    )
  }
  x
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name column_name

column_name <- function(x) scalar(x, "Column name")


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name asset_id

asset_id <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  scalar(x, "Asset id")
  if (!grepl("^[A-Za-z][A-Za-z0-9_.]*$", x)) {
    abort(
      subclass = "dataraft_error_definition",
      "Asset ids must use letters, digits, underscores or dots."
    )
  }
  x
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name uid

uid <- function() {
  rlang::local_error_call(rlang::caller_env())
  paste0(
    "r",
    substr(
      digest::digest(
        list(now(), Sys.getpid(), tempfile("dataraft-id-")),
        algo = "sha256"
      ),
      1,
      24
    )
  )
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name flag

flag <- function(value, name) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    abort(
      subclass = "dataraft_error_definition",
      paste(name, "must be TRUE or FALSE."),
      "dr_invalid_argument"
    )
  }
  value
}

# Keep registry fingerprints stable; report values need full double precision.
