#' Read a source using an interchangeable adapter
#'
#' Data frames and zero-argument functions work directly. File paths are
#' normalized by [dr_add_source()]. A source method returns a data frame or
#' tibble or a lazy table. Use a function to call an API or another existing client.
#' @param source Source object, data frame or zero-argument function.
#' @param ... Adapter-specific options.
#' @returns A data frame or tibble. Connections supplied by callers stay open.
#' @export
#' @examples
#' dr_read_source(function() data.frame(id = 1:2))
dr_read_source <- function(source, ...) UseMethod("dr_read_source")

#' @export
dr_read_source.data.frame <- function(source, ...) source

#' @export
dr_read_source.function <- function(source, ...) source()

#' @export
dr_read_source.dr_source <- function(source, ...) source$reader(source$path)

#' @export
dr_read_source.default <- function(source, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "This source needs a dr_read_source() method. You can also pass a function returning a data frame."
  )
}


#' Execute an interchangeable transformation
#'
#' Ordinary functions receive and return a data frame, tibble or lazy table.
#' With multiple sources, the first transform receives a named list of tables. Extension methods may delegate to existing tools. They must return
#' a data frame or tibble. No transformation class is required for R functions.
#' @param transform Function or adapter object.
#' @param data Input data frame.
#' @param ... Adapter-specific options.
#' @returns A data frame or tibble.
#' @export
#' @examples
#' dr_execute_transform(function(data) transform(data, doubled = amount * 2),
#'   data.frame(amount = 10))
dr_execute_transform <- function(transform, data, ...) {
  UseMethod("dr_execute_transform")
}

#' @export
dr_execute_transform.function <- function(transform, data, ...) transform(data)

#' @export
dr_execute_transform.default <- function(transform, data, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "This transformation needs a dr_execute_transform() method. An ordinary R function also works."
  )
}


# Auxiliary inputs belong to their transformation, not to the primary table.
#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name component_sources

component_sources <- function(x, ...) UseMethod("component_sources")

#' @export
component_sources.default <- function(x, ...) list()


replace_component_sources <- function(x, sources, ...) {
  rlang::local_error_call(rlang::caller_env())
  UseMethod("replace_component_sources")
}

#' @export
replace_component_sources.default <- function(x, sources, ...) {
  rlang::local_error_call(rlang::caller_env())
  if (length(sources)) {
    abort(
      subclass = "dataraft_error_definition",
      "This component cannot replace its dependencies."
    )
  }
  x
}


transform_source_names <- function(step, sources) {
  rlang::local_error_call(rlang::caller_env())
  if (!length(sources)) {
    return(character())
  }
  if (
    is.null(names(sources)) ||
      anyNA(names(sources)) ||
      any(!nzchar(names(sources))) ||
      anyDuplicated(names(sources))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Transformation dependencies must have unique, non-empty names."
    )
  }
  paste0("transform:", step, ":", names(sources))
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name product_sources

product_sources <- function(product) {
  rlang::local_error_call(rlang::caller_env())
  sources <- product$sources
  for (step in names(product$transforms)) {
    auxiliary <- component_sources(product$transforms[[step]])
    labels <- transform_source_names(step, auxiliary)
    if (any(labels %in% names(sources))) {
      abort(
        subclass = "dataraft_error_definition",
        "A source name conflicts with a transformation dependency."
      )
    }
    sources <- c(sources, stats::setNames(auxiliary, labels))
  }
  sources
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name replace_product_sources

replace_product_sources <- function(product, sources) {
  rlang::local_error_call(rlang::caller_env())
  product$sources <- sources[names(product$sources)]
  for (step in names(product$transforms)) {
    auxiliary <- component_sources(product$transforms[[step]])
    if (!length(auxiliary)) {
      next
    }
    labels <- transform_source_names(step, auxiliary)
    product$transforms[[step]] <- replace_component_sources(
      product$transforms[[step]],
      stats::setNames(sources[labels], names(auxiliary))
    )
  }
  product
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name normalize_result_source

normalize_result_source <- function(result) {
  rlang::local_error_call(rlang::caller_env())
  if (!result$status %in% c("completed", "published", "cached")) {
    abort(
      subclass = "dataraft_error_definition",
      "Use a successful run as a source. This run has no approved output."
    )
  }
  pinned <- length(result$release_id) == 1L &&
    !is.na(result$release_id) &&
    nzchar(result$release_id)
  destination <- result$output_config %||% result$output_lake
  if (pinned && !is.null(destination)) {
    source <- dataraft.lake::dr_source_release(
      destination,
      result$asset,
      result$release_id
    )
    source$output_lake <- result$output_lake
    source$run_id <- result$run_id
    return(source)
  }
  if (is.null(result$data)) {
    abort(
      subclass = "dataraft_error_definition",
      "This successful run has neither submitted data nor a readable release reference."
    )
  }
  structure(
    list(data = result$data, asset = result$asset, run_id = result$run_id),
    class = "dr_result_source"
  )
}


#' @export
dr_read_source.dr_result_source <- function(source, ...) {
  data <- source$data
  attr(data, "dr_input_reference") <- list(
    asset = source$asset,
    run_id = source$run_id
  )
  data
}

#' @export
dr_check_component.dr_result_source <- function(x, ...) {
  assert_component(x$data, "dr_read_source")
  invisible(x)
}

#' @export
dr_inspect.dr_result_source <- function(x, ...) {
  data <- dr_inspect(x$data)
  data$rows <- NULL
  list(type = "accepted run", asset = x$asset, data = data)
}


#' Check a component before execution
#'
#' Extension packages implement this S3 generic alongside their read, transform,
#' quality, target or catalog method. Validate configuration and dependencies;
#' do not fetch data, execute user callbacks or write anything. Return `x`
#' invisibly or raise an actionable error. Passing preflight does not prove
#' that remote resources are available or that the data meet their contract.
#' @param x Component to inspect.
#' @param ... Reserved for adapter options.
#' @returns `x`, invisibly, when its configuration is valid.
#' @export
#' @examples
#' dr_check_component(dr_quality_rule("nonnegative", ~ amount >= 0))
dr_check_component <- function(x, ...) UseMethod("dr_check_component")

#' @export
dr_check_component.default <- function(x, ...) {
  abort(
    subclass = "dataraft_error_definition",
    "This component needs a dr_check_component() preflight method."
  )
}

#' @export
dr_check_component.NULL <- function(x, ...) invisible(x)

#' @export
dr_check_component.data.frame <- function(x, ...) {
  if (anyDuplicated(names(x)) || anyNA(names(x)) || any(!nzchar(names(x)))) {
    abort(
      subclass = "dataraft_error_definition",
      "Source column names must be non-empty and unique."
    )
  }
  invisible(x)
}

#' @export
dr_check_component.function <- function(x, ...) invisible(x)

#' @export
dr_check_component.dr_source <- function(x, ...) {
  if (!is.function(x$reader)) {
    abort(
      subclass = "dataraft_error_definition",
      "The file source needs a reader function."
    )
  }
  if (!file.exists(x$path) || dir.exists(x$path)) {
    abort(
      subclass = "dataraft_error_definition",
      "Source file is missing.",
      "dr_missing_delivery"
    )
  }
  if (identical(x$reader, excel_reader)) {
    need("readxl")
  }
  invisible(x)
}

#' @export
dr_check_component.dr_rule <- function(x, ...) {
  scalar(x$name, "rule name")
  if (identical(x$engine, "pointblank")) {
    need("pointblank")
  }
  if (!is.function(x$check) && !inherits(x$check, "formula")) {
    abort(
      subclass = "dataraft_error_definition",
      "The quality rule needs a function or formula."
    )
  }
  if (inherits(x$check, "formula") && length(x$check) != 2L) {
    abort(
      subclass = "dataraft_error_definition",
      "Use a one-sided quality formula, for example ~ amount >= 0."
    )
  }
  invisible(x)
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name component_method

component_method <- function(generic, x) {
  rlang::local_error_call(rlang::caller_env())
  any(vapply(
    class(x),
    function(cl) {
      !is.null(utils::getS3method(generic, cl, optional = TRUE))
    },
    logical(1)
  ))
}


assert_component <- function(x, generic) {
  rlang::local_error_call(rlang::caller_env())
  if (!component_method(generic, x)) {
    abort(
      subclass = "dataraft_error_definition",
      paste0(
        "Component class `",
        class(x)[[1]],
        "` needs a ",
        generic,
        "() method."
      )
    )
  }
  dr_check_component(x)
  invisible(x)
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name frame_result

frame_result <- function(data, label) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.data.frame(data)) {
    abort(
      subclass = "dataraft_error_definition",
      paste0(
        label,
        " did not return a data frame or tibble. Received: ",
        paste(class(data), collapse = "/"),
        "."
      ),
      "dr_component_result"
    )
  }
  dr_check_component.data.frame(data)
  data
}


is_lazy_table <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  inherits(
    x,
    c("tbl_sql", "Table", "RecordBatch", "Dataset", "arrow_dplyr_query")
  )
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name table_result

table_result <- function(data, label) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.data.frame(data) && !is_lazy_table(data)) {
    abort(
      subclass = "dataraft_error_definition",
      paste0(
        label,
        " did not return a data frame or lazy table. Received: ",
        paste(class(data), collapse = "/"),
        ". Combine multiple sources in a transformation before validation."
      ),
      "dr_component_result"
    )
  }
  columns <- if (is_lazy_table(data)) {
    colnames(data) %||% names(data)
  } else {
    names(data)
  }
  if (anyDuplicated(columns) || anyNA(columns) || any(!nzchar(columns))) {
    abort(
      subclass = "dataraft_error_definition",
      paste0(label, " must have unique, non-empty column names.")
    )
  }
  data
}


#' @export
dr_read_source.tbl_sql <- function(source, ...) source

#' @export
dr_check_component.tbl_sql <- function(x, ...) {
  if (!DBI::dbIsValid(dbplyr::remote_con(x))) {
    abort(
      subclass = "dataraft_error_definition",
      "The lazy table connection is closed. Open it before running."
    )
  }
  invisible(table_result(x, "The source"))
}

#' @export
dr_inspect.tbl_sql <- function(x, ...) {
  list(
    type = "lazy database table",
    columns = colnames(x),
    query = as.character(dbplyr::sql_render(x))
  )
}

#' @export
dr_read_source.dr_product <- function(source, ...) {
  result_data(dr_run(source, ...))
}

#' @export
dr_check_component.dr_product <- function(x, ...) invisible(dr_validate(x))
