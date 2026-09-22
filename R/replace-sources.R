#' Replace inputs without rebuilding a workflow
#'
#' Edits definitions only: no readers, transformations, connections or dbt
#' commands run. Execute the returned definition with [dr_run()] or [dr_publish()],
#' or rebuild its [dataraft.adapters::dr_as_targets()] graph to let targets cache unaffected products.
#'
#' For products, names select a delivery name shown by [dr_plan()] or a nested
#' product ID. A product ID updates the ordinary delivery at the end of its
#' single-primary-input chain, retaining every product's transforms, checks and
#' target in every reference, including
#' lookups. Products with multiple primary inputs need an explicit edited
#' definition instead. A replacement product with the same ID explicitly
#' replaces the whole definition. If a root alias also names that same product, the product
#' ID interpretation applies. Other alias/ID collisions are rejected. Pinned
#' results are not product definitions: they change only when their root source
#' alias is explicitly selected. To change a deeper pinned input, replace its
#' containing product with an edited definition. Overlapping edits that discard
#' another requested replacement are rejected.
#'
#' For managed dbt projects, names select existing source table aliases. An alias
#' appearing in more than one group is ambiguous; use `group.table` instead.
#' Replacements must be successful immutable lake results in the same catalog.
#' Unselected bindings retain their exact release IDs. Files are updated only
#' when the project executes. External-profile projects are not supported.
#'
#' @param x A product or managed [dataraft.dbt::dr_dbt_project()] definition.
#' @param ... Named replacement sources, using the same values as [dr_add_source()]
#'   for products. For managed dbt, successful published lake results.
#' @returns An updated definition of the same class as `x`.
#' @export
#' @examples
#' orders <- dr_product("orders", data.frame(amount = 10))
#' totals <- dr_product("totals", orders)
#' corrected <- totals |> dr_replace_sources(orders = data.frame(amount = 20))
#' corrected |> dr_run() |> dr_collect()
dr_replace_sources <- function(x, ...) {
  replace_sources_list(x, list(...))
}


replace_sources_list <- function(x, replacements) {
  dr_replace_source_bindings(x, replacements)
}

#' Replace named source bindings through an extension
#' @param x A product or backend-specific definition.
#' @param replacements Uniquely named list of new source bindings.
#' @returns An updated definition, without executing it.
#' @export
dr_replace_source_bindings <- function(x, replacements) {
  nms <- names(replacements)
  if (
    !length(replacements) ||
      is.null(nms) ||
      anyNA(nms) ||
      any(!nzchar(nms)) ||
      anyDuplicated(nms)
  ) {
    abort(
      subclass = "dataraft_error_source",
      "Supply uniquely named, non-empty replacement sources."
    )
  }
  UseMethod("dr_replace_source_bindings")
}

#' @export
dr_replace_source_bindings.default <- function(x, replacements) {
  rlang::local_error_call(rlang::caller_env())
  nms <- names(replacements)
  if (!inherits(x, "dr_product")) {
    abort(
      subclass = "dataraft_error_source",
      "x must be a product or a managed dbt project definition."
    )
  }
  if (inherits(x, "dr_model_product")) {
    if (!all(nms %in% names(x$sources))) {
      abort(
        subclass = "dataraft_error_source",
        paste(
          "Unknown model table. Choose:",
          paste(names(x$sources), collapse = ", ")
        )
      )
    }
    for (name in nms) {
      value <- replacements[[name]]
      x$sources[[name]] <- if (inherits(value, "dr_product")) {
        value
      } else {
        replace_primary_delivery(x$sources[[name]], value)
      }
    }
    return(x)
  }
  definitions <- replacement_graph(x)
  ids <- setdiff(names(definitions), x$id)
  sources <- product_sources(x)
  aliases <- delivery_aliases(x)
  # Resolve public delivery names to execution dependencies once, preserving
  # product-ID replacement across every reference in the existing graph.
  for (i in seq_along(nms)) {
    name <- nms[[i]]
    if (name %in% names(aliases)) {
      path <- unname(aliases[[name]])
      if (name %in% ids && !identical(name, path)) {
        if (
          !(inherits(sources[[path]], "dr_product") &&
            identical(sources[[path]]$id, name))
        ) {
          abort(
            subclass = "dataraft_error_source",
            paste("Ambiguous delivery name and product ID:", name)
          )
        }
      } else {
        nms[[i]] <- path
      }
    }
  }
  if (anyDuplicated(nms)) {
    abort(
      subclass = "dataraft_error_source",
      "The same delivery was selected more than once. Use one name per replacement."
    )
  }
  names(replacements) <- nms
  modes <- stats::setNames(rep("alias", length(nms)), nms)
  for (name in nms) {
    alias <- name %in% names(sources)
    id <- name %in% ids
    if (!alias && !id) {
      abort(
        subclass = "dataraft_error_source",
        paste0(
          "Unknown replacement source: ",
          name,
          ". Available names: ",
          paste(sort(unique(c(names(aliases), ids))), collapse = ", "),
          "."
        )
      )
    }
    if (
      alias &&
        id &&
        !(inherits(sources[[name]], "dr_product") &&
          identical(sources[[name]]$id, name))
    ) {
      abort(
        subclass = "dataraft_error_source",
        paste("Ambiguous root alias and product ID:", name)
      )
    }
    if (id) {
      modes[[name]] <- "id"
    }
    replacement <- replacements[[name]]
    if (id) {
      if (inherits(replacement, "dr_product")) {
        if (!identical(replacement$id, name)) {
          abort(
            subclass = "dataraft_error_source",
            paste(
              "A replacement product must retain the selected ID:",
              name
            )
          )
        }
      } else {
        replacement <- replace_primary_delivery(
          definitions[[name]],
          replacement
        )
      }
    }
    if (
      !id &&
        inherits(sources[[name]], "dr_product") &&
        !inherits(replacement, "dr_product")
    ) {
      replacement <- replace_primary_delivery(sources[[name]], replacement)
    }
    replacements[[name]] <- normalize_source(replacement, x$id, name)
  }
  applied <- character()
  visit <- function(node, root = FALSE) {
    rlang::local_error_call(rlang::caller_env())
    inputs <- product_sources(node)
    for (alias in names(inputs)) {
      source <- inputs[[alias]]
      selected <- if (root && alias %in% nms && modes[[alias]] == "alias") {
        alias
      } else if (
        inherits(source, "dr_product") &&
          source$id %in% nms &&
          modes[[source$id]] == "id"
      ) {
        source$id
      } else {
        NULL
      }
      if (!is.null(selected)) {
        inputs[[alias]] <- replacements[[selected]]
        applied <<- union(applied, selected)
      } else if (inherits(source, "dr_product")) {
        inputs[[alias]] <- visit(source)
      }
    }
    replace_product_sources(node, inputs)
  }
  out <- visit(x, root = TRUE)
  unused <- setdiff(nms, applied)
  if (length(unused)) {
    abort(
      subclass = "dataraft_error_source",
      paste(
        "Overlapping replacements discard requested sources:",
        paste(unused, collapse = ", ")
      )
    )
  }
  replacement_graph(out)
  out
}


delivery_aliases <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  aliases <- stats::setNames(names(x$sources), names(x$sources))
  sources <- product_sources(x)
  for (step in names(x$transforms)) {
    transform <- x$transforms[[step]]
    paths <- transform_source_names(step, component_sources(transform))
    labels <- if (inherits(transform, "dr_lookup_transform")) {
      transform$name %||% step
    } else {
      paths
    }
    if (any(labels %in% names(aliases))) {
      if (
        length(labels) == 1L &&
          same_delivery_product(
            sources[[aliases[[labels]]]],
            sources[[paths]],
            labels
          )
      ) {
        next
      }
      abort(
        subclass = "dataraft_error_source",
        "Delivery names must be unique. Rename the lookup with dr_add_lookup(name = )."
      )
    }
    aliases <- c(aliases, stats::setNames(paths, labels))
  }
  conflicting <- names(aliases) %in%
    names(sources) &
    names(aliases) != unname(aliases)
  if (any(conflicting)) {
    abort(
      subclass = "dataraft_error_source",
      "A delivery name conflicts with another source. Supply a different lookup name."
    )
  }
  aliases
}


same_delivery_product <- function(a, b, name) {
  rlang::local_error_call(rlang::caller_env())
  inherits(a, "dr_product") &&
    inherits(b, "dr_product") &&
    identical(a$id, name) &&
    identical(b$id, name)
}


replace_primary_delivery <- function(product, replacement) {
  rlang::local_error_call(rlang::caller_env())
  if (length(product$sources) != 1L) {
    abort(
      subclass = "dataraft_error_source",
      paste0(
        "Replacing a product input requires exactly one primary source at `",
        product$id,
        "`. Use sources = list(name = value) to select a deeper input or supply an edited product definition."
      )
    )
  }
  source <- product$sources[[1L]]
  if (inherits(source, "dr_product")) {
    replacement <- replace_primary_delivery(source, replacement)
  }
  dr_add_source(product, replacement, replace = TRUE)
}


replacement_graph <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  definitions <- list()
  visit <- function(node, stack = character()) {
    rlang::local_error_call(rlang::caller_env())
    if (node$id %in% stack) {
      abort(
        subclass = "dataraft_error_source",
        paste(
          "Product dependency cycle:",
          paste(c(stack, node$id), collapse = " -> ")
        )
      )
    }
    if (node$id %in% names(definitions)) {
      if (!identical(definitions[[node$id]], node)) {
        abort(
          subclass = "dataraft_error_source",
          paste("Different product definitions share the ID:", node$id)
        )
      }
      return(invisible(NULL))
    }
    for (source in product_sources(node)) {
      if (inherits(source, "dr_product")) visit(source, c(stack, node$id))
    }
    definitions[[node$id]] <<- node
    invisible(NULL)
  }
  visit(x)
  definitions
}
