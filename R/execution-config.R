#' Reuse explicit execution defaults
#'
#' Define execution choices once and pass the value to [dr_run()], [dr_publish()] or
#' [dataraft.lake::dr_ingest()], or store it once with `dr_product(execution = )`. Construction
#' neither reads sources nor opens destinations. Stored defaults require
#' connection-free destinations and are used only for the root definition.
#' Engine defaults apply recursively to dependencies without changing the
#' original definitions. Explicit rule and lookup engines always win. Only
#' ordinary formula rules inherit the quality default; custom functions and
#' agent builders retain their own semantics.
#'
#' A destination fills the root product's missing target only. Dependencies
#' without targets remain in memory. Configured targets and their layers remain
#' unchanged. The layer default applies to a newly supplied root lake destination
#' or a root target with no layer. Explicit
#' `dr_publish(to = , layer = )` arguments override the root product only.
#' Ingestion always uses the raw layer and rejects another layer default.
#' @param quality Default formula engine: `"native"` or `"pointblank"`.
#' @param relationships Default checked lookup engine: `"native"` or `"dm"`.
#' @param to Optional target adapter, lake configuration or local lake folder.
#' @param layer Optional default lake publication layer.
#' @returns An ordinary execution configuration value. No global state is changed.
#' @export
#' @examples
#' execution <- dr_execution_config()
#' dr_product("orders", data.frame(amount = c(10, 20))) |>
#'   dr_add_quality(~ amount > 0) |>
#'   dr_run(execution = execution) |>
#'   dr_collect()
dr_execution_config <- function(
  quality = "native",
  relationships = "dm",
  to = NULL,
  layer = NULL
) {
  scalar(quality, "quality")
  scalar(relationships, "relationships")
  normalize_quality_engine(quality)
  relationships <- match.arg(relationships, "dm")
  if (!is.null(layer)) {
    ident(layer)
  }
  if (!is.null(to)) {
    to <- normalize_target(to)
    if (!is.null(layer)) {
      to <- dr_configure_target(to, layer = layer)
    }
    if (
      !component_method("dr_write_target", to) &&
        !component_method("dr_execute_target", to)
    ) {
      abort(
        subclass = "dataraft_error_definition",
        "The execution target needs a dr_write_target() method."
      )
    }
  }
  structure(
    list(
      quality = quality,
      relationships = relationships,
      to = to,
      layer = layer
    ),
    class = "dr_execution_config"
  )
}


validate_execution_config <- function(execution) {
  rlang::local_error_call(rlang::caller_env())
  if (is.null(execution)) {
    return(NULL)
  }
  if (
    !inherits(execution, "dr_execution_config") ||
      !identical(names(execution), c("quality", "relationships", "to", "layer"))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "execution must be an dr_execution_config() value."
    )
  }
  do.call(dr_execution_config, unclass(execution))
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name apply_execution_defaults

apply_execution_defaults <- function(product, execution) {
  rlang::local_error_call(rlang::caller_env())
  execution <- validate_execution_config(execution)
  if (is.null(execution)) {
    return(product)
  }
  resolve_rule <- function(rule) {
    rlang::local_error_call(rlang::caller_env())
    if (
      identical(class(rule), "dr_rule") &&
        inherits(rule$check, "formula") &&
        identical(rule$engine_explicit, FALSE)
    ) {
      rule$engine <- normalize_quality_engine(execution$quality)
    }
    rule
  }
  visit <- function(x, stack = character()) {
    rlang::local_error_call(rlang::caller_env())
    if (x$id %in% stack) {
      abort(
        subclass = "dataraft_error_definition",
        paste0(
          "Product dependency cycle: ",
          paste(c(stack, x$id), collapse = " -> "),
          "."
        ),
        "dr_dependency_cycle"
      )
    }
    x <- editable_product(x)
    x$quality <- lapply(x$quality, resolve_rule)
    if (!is.null(x$contract)) {
      rules <- lapply(x$contract$rules, resolve_rule)
      x$execution_contract_rules <- if (!identical(rules, x$contract$rules)) {
        rules
      } else {
        NULL
      }
    }
    for (name in names(x$transforms)) {
      step <- x$transforms[[name]]
      if (
        identical(class(step), "dr_lookup_transform") &&
          identical(step$engine_explicit, FALSE)
      ) {
        step$engine <- execution$relationships
        x$transforms[[name]] <- step
      }
    }
    if (!length(stack) && is.null(x$target) && !is.null(execution$to)) {
      x$target <- execution$to
      if (!is.null(execution$layer)) {
        x$target <- dr_configure_target(x$target, layer = execution$layer)
      }
    } else if (
      !length(stack) && is.null(x$target$layer) && !is.null(execution$layer)
    ) {
      x$target <- dr_configure_target(x$target, layer = execution$layer)
    }
    sources <- lapply(product_sources(x), function(source) {
      if (inherits(source, "dr_product")) {
        visit(source, c(stack, x$id))
      } else {
        source
      }
    })
    replace_product_sources(x, sources)
  }
  visit(product)
}


validate_stored_execution <- function(execution) {
  rlang::local_error_call(rlang::caller_env())
  execution <- validate_execution_config(execution)
  has_connection <- function(x) {
    rlang::local_error_call(rlang::caller_env())
    if (inherits(x, c("dr_lake", "DBIConnection"))) {
      return(TRUE)
    }
    if (is.list(x)) {
      return(any(vapply(x, has_connection, logical(1))))
    }
    FALSE
  }
  if (has_connection(execution)) {
    abort(
      subclass = "dataraft_error_definition",
      "Stored execution defaults cannot contain an open connection. Use a dr_lake_config(), folder, or connection factory."
    )
  }
  execution
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name product_execution

product_execution <- function(x, execution) {
  rlang::local_error_call(rlang::caller_env())
  validate_execution_config(
    execution %||% attr(x, "dr_execution_config", exact = TRUE)
  )
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @name replace_execution_sources

replace_execution_sources <- function(x, data = NULL, sources = NULL) {
  rlang::local_error_call(rlang::caller_env())
  if (!is.null(sources) && (!is.list(sources) || is.data.frame(sources))) {
    abort(
      subclass = "dataraft_error_definition",
      "sources must be a named list, for example sources = list(orders = new_orders)."
    )
  }
  if (
    !is.null(data) &&
      inherits(x, "dr_product") &&
      !inherits(x, "dr_model_product") &&
      !length(x$sources)
  ) {
    x <- dr_add_source(x, data, name = x$id)
  }
  if (!is.null(data)) {
    if (!inherits(x, "dr_product") || length(x$sources) != 1L) {
      abort(
        subclass = "dataraft_error_definition",
        "data requires a product with exactly one primary input. Use sources = list(name = value) for named inputs."
      )
    }
    replacement_graph(x)
    leaf <- x
    while (inherits(leaf$sources[[1L]], "dr_product")) {
      leaf <- leaf$sources[[1L]]
      if (length(leaf$sources) != 1L) {
        abort(
          subclass = "dataraft_error_definition",
          paste0(
            "Replacing a product input requires exactly one primary source at `",
            leaf$id,
            "`. Use sources = list(name = value) to select an input."
          )
        )
      }
    }
    # Select the deepest product globally, so references from lookup branches
    # receive the same delivery and retain one consistent definition per ID.
    name <- if (identical(leaf$id, x$id)) names(x$sources)[[1L]] else leaf$id
    if (name %in% names(sources)) {
      abort(
        subclass = "dataraft_error_definition",
        paste0(
          "Delivery '",
          name,
          "' was supplied in both data and sources. Supply it once, not both."
        )
      )
    }
    sources <- c(stats::setNames(list(data), name), sources)
  }
  if (!is.null(sources)) {
    if (!is.list(sources) || is.data.frame(sources)) {
      abort(
        subclass = "dataraft_error_definition",
        "sources must be a named list, for example sources = list(orders = new_orders)."
      )
    }
    x <- replace_sources_list(x, sources)
  }
  x
}
