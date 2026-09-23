#' Define a reusable data preparation recipe
#'
#' A recipe contains ordered transformations, independently of product identity,
#' primary inputs, output contracts and destinations. Construction never reads
#' data. Attach it with [dr_add_recipe()] to a product or [dr_workflow()]. Recipes are
#' ordinary R values: adding a step returns a new value and leaves the original
#' unchanged. Expressions use dplyr data masking and retain their environments.
#'
#' These are data preparation instructions, not fitted preprocessing models.
#' There is no training or implicit `prep()`/`bake()` phase. Use [dr_trial()] on
#' the assembled workflow to inspect checked output, then [dr_publish()] to save it.
#' @returns A recipe specification.
#' @export
#' @examples
#' preparation <- dr_recipe() |>
#'   dr_step_mutate(amount = round(amount, 2)) |>
#'   dr_step_filter(amount >= 0)
#' dr_product("orders", data.frame(amount = c(10.123, 20))) |>
#'   dr_add_recipe(preparation) |>
#'   dr_trial() |>
#'   dr_collect()
dr_recipe <- function() {
  structure(list(steps = list()), class = "dr_recipe")
}


assert_recipe <- function(x) {
  rlang::local_error_call(rlang::caller_env())
  if (!inherits(x, "dr_recipe")) {
    abort(
      subclass = "dataraft_error_definition",
      "Start with dr_recipe() before adding preparation steps."
    )
  }
  invisible(x)
}


#' Add a deferred preparation step
#'
#' @section Available steps:
#' Use `dr_step_mutate()` and `dr_step_rename()` to change columns;
#' `dr_step_filter()` and `dr_step_select()` to select rows and columns;
#' `dr_step_arrange()` and `dr_step_distinct()` to order and deduplicate;
#' and `dr_step_summarise()` to aggregate.
#'
#' @section Execution:
#' Steps execute in addition order using the existing dplyr and transformation
#' adapters. No implicit collection is performed. Use `dr_step_transform()` for
#' an ordinary function, formula using `.x`, or an existing transform adapter.
#' It also accepts an engine-configured [dr_lookup_spec()].
#' @param x A [dr_recipe()] specification.
#' @param ... Arguments passed to the corresponding dplyr verb at execution.
#' @param transform Function, formula using `.x`, or transformation adapter.
#' @param name Optional unique step name for `dr_step_transform()`.
#' @param .by,.preserve,.groups,.keep_all Arguments with their dplyr meanings.
#' @returns An updated recipe. The original is unchanged.
#' @export
#' @examples
#' dr_recipe() |>
#'   dr_step_mutate(net = gross / 1.19) |>
#'   dr_step_select(id, net)
dr_step_transform <- function(x, transform, name = NULL) {
  assert_recipe(x)
  holder <- dr_product("recipe")
  holder$transforms <- x$steps
  holder <- dr_add_transform(holder, transform, name)
  x$steps <- holder$transforms
  x
}


recipe_dplyr_step <- function(x, verb, args) {
  rlang::local_error_call(rlang::caller_env())
  assert_recipe(x)
  dr_step_transform(
    x,
    structure(list(verb = verb, args = args), class = "dr_dplyr_transform"),
    name = paste0(verb, "_", length(x$steps) + 1L)
  )
}


#' @rdname dr_step_transform
#' @export
dr_step_mutate <- function(x, ...) {
  recipe_dplyr_step(x, "mutate", rlang::enquos(...))
}


#' @rdname dr_step_transform
#' @export
dr_step_filter <- function(x, ..., .by = NULL, .preserve = FALSE) {
  args <- rlang::enquos(...)
  if (!missing(.by)) {
    args$.by <- rlang::enquo(.by)
  }
  if (!missing(.preserve)) {
    args$.preserve <- rlang::enquo(.preserve)
  }
  recipe_dplyr_step(x, "filter", args)
}


#' @rdname dr_step_transform
#' @export
dr_step_select <- function(x, ...) {
  recipe_dplyr_step(x, "select", rlang::enquos(...))
}


#' @rdname dr_step_transform
#' @export
dr_step_rename <- function(x, ...) {
  recipe_dplyr_step(x, "rename", rlang::enquos(...))
}


#' @rdname dr_step_transform
#' @export
dr_step_arrange <- function(x, ...) {
  recipe_dplyr_step(x, "arrange", rlang::enquos(...))
}


#' @rdname dr_step_transform
#' @export
dr_step_summarise <- function(x, ..., .by = NULL, .groups = NULL) {
  args <- rlang::enquos(...)
  if (!missing(.by)) {
    args$.by <- rlang::enquo(.by)
  }
  if (!missing(.groups)) {
    args$.groups <- rlang::enquo(.groups)
  }
  recipe_dplyr_step(x, "summarise", args)
}


#' @rdname dr_step_transform
#' @export
dr_step_distinct <- function(x, ..., .keep_all = FALSE) {
  args <- rlang::enquos(...)
  if (!missing(.keep_all)) {
    args$.keep_all <- rlang::enquo(.keep_all)
  }
  recipe_dplyr_step(x, "distinct", args)
}


#' Add a checked lookup to a recipe
#'
#' Uses the same dependency resolution and relationship checks as [dr_lookup_spec()].
#' Reusable lookup specifications can instead be passed to [dr_step_transform()].
#' @inheritParams dr_lookup_spec
#' @param engine Constraint engine: `"native"` or optional `"dm"`.
#' @param x A [dr_recipe()] specification.
#' @returns An updated recipe.
#' @export
#' @examples
#' dr_recipe() |>
#'   dr_step_lookup(data.frame(id = 1:2, region = c("North", "South")),
#'     by = "id", name = "customers")
dr_step_lookup <- function(
  x,
  source,
  by,
  engine = c("dm", "native"),
  unmatched = c("error", "keep"),
  suffix = c(".x", ".y"),
  name = NULL,
  table = NULL
) {
  assert_recipe(x)
  if (is.null(name)) {
    name <- if (!is.null(table)) {
      table
    } else if (inherits(source, "dr_product")) {
      source$id
    } else if (is.symbol(substitute(source))) {
      as.character(substitute(source))
    }
  }
  holder <- dr_product("recipe")
  holder$transforms <- x$steps
  args <- list(
    x = holder,
    source = source,
    by = by,
    unmatched = match.arg(unmatched),
    suffix = suffix,
    name = name,
    table = table
  )
  if (!missing(engine)) {
    args$engine <- match.arg(engine)
  }
  x$steps <- do.call(dr_add_lookup, args)$transforms
  x
}


#' @export
dr_inspect.dr_recipe <- function(x, ...) {
  list(type = "recipe", steps = lapply(x$steps, dr_inspect))
}


#' @export
print.dr_recipe <- function(x, ...) {
  cat("<recipe> ", length(x$steps), " preparation step(s)\n", sep = "")
  if (length(x$steps)) {
    print(tibble::tibble(
      step = names(x$steps),
      operation = vapply(x$steps, \(step) dr_inspect(step)$type, character(1))
    ))
  }
  invisible(x)
}
