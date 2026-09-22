#' Use dplyr verbs in a product definition
#'
#' Ordinary dplyr verbs append deferred transformations to a product. They use
#' the same data masking, tidy selection and grouping rules as dplyr on the
#' acquired table. Nothing is read until [dr_run()] or [dr_publish()].
#'
#' Expressions retain their R environments. Inspection records expressions,
#' plus non-disclosing hashes of referenced lexical bindings. Known input columns
#' take precedence over ambient names; use `.env$name` for explicit lexical
#' bindings and `.data$name` for columns. Changing an external binding can change
#' a later run: use an explicit `code_version` when enabling lake caching, and
#' an explicit targets cue for dynamic environment or external state. To freeze
#' a small value in an expression, inject it with `!!`.
#'
#' Use [dr_step_transform()] for other functions. Multiple primary sources must
#' first be combined by a transformation; [dr_step_lookup()] adds a checked
#' auxiliary source without changing the primary table.
#' `left_join()` on a product gives guidance rather than guessing relationship
#' rules. Use `dr_add_lookup(reference, by = ..., name = "reference")` for checked
#' enrichment, or join ordinary tables inside `dr_add_transform()` for other joins.
#' @param .data,x Product definition.
#' @param ... Arguments captured and passed to the corresponding dplyr verb.
#' @param .by,.preserve,.by_group,.add,.drop,.groups,.keep_all,.before,.after,wt,sort,name
#'   Arguments with their usual dplyr meanings, evaluated during execution.
#' @returns An updated product definition.
#' @name product-dplyr
#' @importFrom dplyr mutate filter select rename relocate arrange group_by ungroup summarise distinct count left_join
#' @examples
#' orders <- dr_product("orders", data.frame(group = c("a", "a"), amount = c(10, 20))) |>
#'   dplyr::mutate(tax = amount * 0.2) |>
#'   dplyr::summarise(total = sum(amount), .by = group)
#' orders |> dr_run() |> dr_collect()
NULL


#' @export
left_join.dr_product <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...
) {
  rlang::local_error_call(rlang::caller_env())
  abort(
    subclass = "dataraft_error_definition",
    paste(
      "To enrich a product with a reference table, use dr_add_lookup(reference, by = ..., name = \"reference\").",
      "It checks unique reference keys and matching input keys.",
      "For other join relationships, use dplyr::left_join() on ordinary tables inside dr_add_transform()."
    )
  )
}


dplyr_product_step <- function(x, verb, args) {
  rlang::local_error_call(rlang::caller_env())
  # A column wins over an identically named ambient binding in a data mask.
  # Use only known schemas here; never read a source during definition.
  masked <- unique(c(
    unlist(
      lapply(x$sources, function(source) {
        if (is.data.frame(source)) names(source) else character()
      }),
      use.names = FALSE
    ),
    names(x$contract$columns),
    unlist(
      lapply(x$transforms, function(step) {
        if (inherits(step, "dr_dplyr_transform")) {
          c(step$masked, names(step$args))
        } else {
          character()
        }
      }),
      use.names = FALSE
    )
  ))
  dr_add_transform(
    x,
    structure(
      list(verb = verb, args = args, masked = masked),
      class = "dr_dplyr_transform"
    ),
    name = paste0(verb, "_", length(x$transforms) + 1L)
  )
}


#' @rdname product-dplyr
#' @export
mutate.dr_product <- function(.data, ...) {
  rlang::local_error_call(rlang::caller_env())
  dplyr_product_step(.data, "mutate", rlang::enquos(...))
}

#' @rdname product-dplyr
filter.dr_product <- function(.data, ..., .by = NULL, .preserve = FALSE) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(.by)) {
    args$.by <- rlang::enquo(.by)
  }
  if (!missing(.preserve)) {
    args$.preserve <- rlang::enquo(.preserve)
  }
  dplyr_product_step(.data, "filter", args)
}

#' @rdname product-dplyr
#' @export
select.dr_product <- function(.data, ...) {
  rlang::local_error_call(rlang::caller_env())
  dplyr_product_step(.data, "select", rlang::enquos(...))
}

#' @rdname product-dplyr
#' @export
rename.dr_product <- function(.data, ...) {
  rlang::local_error_call(rlang::caller_env())
  dplyr_product_step(.data, "rename", rlang::enquos(...))
}

#' @rdname product-dplyr
#' @export
relocate.dr_product <- function(.data, ..., .before = NULL, .after = NULL) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(.before)) {
    args$.before <- rlang::enquo(.before)
  }
  if (!missing(.after)) {
    args$.after <- rlang::enquo(.after)
  }
  dplyr_product_step(.data, "relocate", args)
}

#' @rdname product-dplyr
#' @export
arrange.dr_product <- function(.data, ..., .by_group = FALSE) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(.by_group)) {
    args$.by_group <- rlang::enquo(.by_group)
  }
  dplyr_product_step(.data, "arrange", args)
}

#' @rdname product-dplyr
#' @export
group_by.dr_product <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(.add)) {
    args$.add <- rlang::enquo(.add)
  }
  if (!missing(.drop)) {
    args$.drop <- rlang::enquo(.drop)
  }
  dplyr_product_step(.data, "group_by", args)
}

#' @rdname product-dplyr
#' @export
ungroup.dr_product <- function(x, ...) {
  rlang::local_error_call(rlang::caller_env())
  dplyr_product_step(x, "ungroup", rlang::enquos(...))
}

#' @rdname product-dplyr
#' @export
summarise.dr_product <- function(.data, ..., .by = NULL, .groups = NULL) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(.by)) {
    args$.by <- rlang::enquo(.by)
  }
  if (!missing(.groups)) {
    args$.groups <- rlang::enquo(.groups)
  }
  dplyr_product_step(.data, "summarise", args)
}

#' @rdname product-dplyr
#' @export
distinct.dr_product <- function(.data, ..., .keep_all = FALSE) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(.keep_all)) {
    args$.keep_all <- rlang::enquo(.keep_all)
  }
  dplyr_product_step(.data, "distinct", args)
}

#' @rdname product-dplyr
#' @export
count.dr_product <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
  rlang::local_error_call(rlang::caller_env())
  args <- rlang::enquos(...)
  if (!missing(wt)) {
    args$wt <- rlang::enquo(wt)
  }
  if (!missing(sort)) {
    args$sort <- rlang::enquo(sort)
  }
  if (!missing(name)) {
    args$name <- rlang::enquo(name)
  }
  dplyr_product_step(x, "count", args)
}


#' @export
dr_execute_transform.dr_dplyr_transform <- function(transform, data, ...) {
  table_result(data, paste0("dplyr::", transform$verb, "() input"))
  args <- as.list(transform$args)
  ordinary <- switch(
    transform$verb,
    mutate = ".keep",
    filter = ".preserve",
    arrange = c(".by_group", ".locale"),
    group_by = c(".add", ".drop"),
    summarise = ".groups",
    distinct = ".keep_all",
    count = c("sort", "name", ".drop"),
    character()
  )
  for (name in intersect(names(args), ordinary)) {
    args[name] <- list(rlang::eval_tidy(args[[name]]))
  }
  implementation <- getExportedValue("dplyr", transform$verb)
  rlang::inject(implementation(data, !!!args))
}

#' @export
dr_check_component.dr_dplyr_transform <- function(x, ...) {
  if (
    !x$verb %in%
      c(
        "mutate",
        "filter",
        "select",
        "rename",
        "relocate",
        "arrange",
        "group_by",
        "ungroup",
        "summarise",
        "distinct",
        "count"
      )
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Unknown deferred dplyr verb."
    )
  }
  invisible(x)
}

#' @export
dr_inspect.dr_dplyr_transform <- function(x, ...) {
  list(
    type = paste0("dplyr::", x$verb),
    arguments = canonical(x$args, masked = x$masked %||% character())
  )
}

#' @export
dr_capabilities.dr_dplyr_transform <- function(x, ...) {
  dr_component_capabilities(lazy = TRUE)
}
