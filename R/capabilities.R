#' Inspect adapter capabilities
#'
#' Adapters report the same six logical fields. `TRUE` means supported, `FALSE`
#' means unsupported, and `NA` means undeclared or dependent on user code.
#' Capabilities describe behavior; they do not test remote permissions.
#' Extension methods can use [dr_component_capabilities()] for a stable shape.
#' An ordinary function may support lazy tables, but its body is not inspected.
#' @param x Source, transform, target or connected lake.
#' @param ... Reserved for adapter options.
#' @returns A named list of logical capabilities.
#' @export
#' @examples
#' dr_capabilities(data.frame(id = 1L))
#' dr_capabilities(NULL)
dr_capabilities <- function(x, ...) UseMethod("dr_capabilities")


#' @rdname dr_capabilities
#' @param read,write,lazy,transactions,partition,immutable Logical scalars.
#' @export
dr_component_capabilities <- function(
  read = NA,
  write = NA,
  lazy = NA,
  transactions = NA,
  partition = NA,
  immutable = NA
) {
  values <- list(
    read = read,
    write = write,
    lazy = lazy,
    transactions = transactions,
    partition = partition,
    immutable = immutable
  )
  if (
    !all(vapply(
      values,
      function(value) is.logical(value) && length(value) == 1L,
      logical(1)
    ))
  ) {
    abort(
      subclass = "dataraft_error_definition",
      "Each capability must be TRUE, FALSE or NA."
    )
  }
  values
}

#' @export
dr_capabilities.default <- function(x, ...) dr_component_capabilities()

#' @export
dr_capabilities.NULL <- function(x, ...) {
  dr_component_capabilities(
    read = FALSE,
    write = TRUE,
    lazy = TRUE,
    transactions = FALSE,
    partition = FALSE,
    immutable = FALSE
  )
}

#' @export
dr_capabilities.data.frame <- function(x, ...) {
  dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = FALSE,
    transactions = FALSE,
    partition = FALSE,
    immutable = FALSE
  )
}

#' @export
dr_capabilities.function <- function(x, ...) {
  dr_component_capabilities(read = TRUE, write = FALSE)
}

#' @export
dr_capabilities.tbl_sql <- function(x, ...) {
  dr_component_capabilities(read = TRUE, write = FALSE, lazy = TRUE)
}

#' @export
dr_capabilities.dr_source <- function(x, ...) {
  dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = FALSE,
    transactions = FALSE,
    partition = FALSE,
    immutable = FALSE
  )
}

#' @export
dr_capabilities.dr_result_source <- function(x, ...) {
  dr_component_capabilities(
    read = TRUE,
    write = FALSE,
    lazy = is_lazy_table(x$data),
    immutable = FALSE
  )
}

#' @export
dr_capabilities.dr_product <- function(x, ...) {
  dr_component_capabilities(
    read = TRUE,
    write = !is.null(x$target),
    lazy = dr_capabilities(x$target)$lazy
  )
}
