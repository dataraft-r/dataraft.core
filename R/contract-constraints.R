#' @importFrom rlang .data .env
NULL

contract_constraint_rules <- function(constraints, columns, required, key) {
  if (
    !is.list(constraints) ||
      (length(constraints) &&
        (is.null(names(constraints)) ||
          anyDuplicated(names(constraints)) ||
          !all(names(constraints) %in% names(columns))))
  ) {
    abort(
      "constraints must be named lists for declared columns.",
      subclass = "dataraft_error_contract"
    )
  }
  rules <- list()
  for (column in names(constraints)) {
    spec <- constraints[[column]]
    allowed <- c("nullable", "min", "max", "enum", "timezone", "precision")
    if (
      !is.list(spec) ||
        is.null(names(spec)) ||
        anyDuplicated(names(spec)) ||
        !all(names(spec) %in% allowed)
    ) {
      abort(
        "Each constraint must name nullable, min, max, enum, timezone or precision.",
        subclass = "dataraft_error_contract"
      )
    }
    if ("nullable" %in% names(spec)) {
      if (
        !is.logical(spec$nullable) ||
          length(spec$nullable) != 1L ||
          is.na(spec$nullable)
      ) {
        abort(
          "nullable must be TRUE or FALSE.",
          subclass = "dataraft_error_contract"
        )
      }
      if (spec$nullable && column %in% key) {
        abort(
          "Key columns cannot be nullable.",
          subclass = "dataraft_error_contract"
        )
      }
      required <- if (spec$nullable) {
        setdiff(required, column)
      } else {
        union(required, column)
      }
    }
    type <- columns[[column]]
    for (bound in intersect(c("min", "max"), names(spec))) {
      value <- spec[[bound]]
      if (
        !type %in% c("numeric", "integer", "Date", "POSIXct") ||
          length(value) != 1L ||
          is.na(value) ||
          !is.finite(value) ||
          !contract_type_matches(value, type)
      ) {
        abort(
          "Bounds must be finite scalar values matching a numeric or temporal column type.",
          subclass = "dataraft_error_contract"
        )
      }
    }
    if (all(c("min", "max") %in% names(spec)) && spec$min > spec$max) {
      abort(
        "Constraint min must not exceed max.",
        subclass = "dataraft_error_contract"
      )
    }
    if (
      "enum" %in%
        names(spec) &&
        (!length(spec$enum) ||
          anyNA(spec$enum) ||
          !contract_type_matches(spec$enum, type) ||
          type == "list")
    ) {
      abort(
        "enum must contain nonmissing values matching the column type.",
        subclass = "dataraft_error_contract"
      )
    }
    if (
      "timezone" %in%
        names(spec) &&
        (type != "POSIXct" ||
          !is.character(spec$timezone) ||
          length(spec$timezone) != 1L ||
          is.na(spec$timezone) ||
          !spec$timezone %in% c("UTC", "GMT", OlsonNames()))
    ) {
      abort(
        "timezone requires a POSIXct column and a known timezone name.",
        subclass = "dataraft_error_contract"
      )
    }
    if (
      "precision" %in%
        names(spec) &&
        (!type %in% c("numeric", "integer", "POSIXct") ||
          !is.numeric(spec$precision) ||
          length(spec$precision) != 1L ||
          is.na(spec$precision) ||
          !is.finite(spec$precision) ||
          spec$precision != trunc(spec$precision) ||
          spec$precision < 0 ||
          spec$precision > 6)
    ) {
      abort(
        "precision must be a whole number from 0 to 6 for numeric or POSIXct columns.",
        subclass = "dataraft_error_contract"
      )
    }
    for (kind in setdiff(names(spec), "nullable")) {
      rules[[length(rules) + 1L]] <- contract_constraint_rule(
        column,
        kind,
        spec[[kind]],
        type
      )
    }
  }
  list(rules = rules, required = required)
}

contract_constraint_rule <- function(column, kind, value, type) {
  force(column)
  force(kind)
  force(value)
  force(type)
  name <- paste("constraint", column, kind, sep = ":")
  if (kind %in% c("min", "max", "enum")) {
    field <- rlang::expr(.data[[!!column]])
    expression <- switch(
      kind,
      min = rlang::expr(is.na(!!field) | !!field >= .env$value),
      max = rlang::expr(is.na(!!field) | !!field <= .env$value),
      enum = rlang::expr(is.na(!!field) | !!field %in% .env$value)
    )
    predicate <- rlang::new_formula(NULL, expression, env = environment())
    return(dr_quality_rule(name, predicate))
  }
  # Driver prototypes do not reliably preserve timezone and decimal semantics.
  # These constraints deliberately produce blocking evidence for lazy inputs.
  dr_quality_rule(name, function(data) {
    if (is_lazy_table(data)) {
      abort(
        "Timezone and precision constraints require collected data; driver metadata cannot prove these properties.",
        subclass = "dataraft_error_contract_constraint_backend"
      )
    }
    x <- data[[column]]
    if (kind == "timezone") {
      zone <- attr(x, "tzone")
      return(rep(identical(zone, value), nrow(data)))
    }
    if (inherits(x, "POSIXct")) {
      x <- as.numeric(x)
    }
    # Compare rounded values using a small absolute tolerance, not a tolerance
    # proportional to the magnitude (which would accept cents on large amounts).
    is.na(x) | (is.finite(x) & abs(x - round(x, digits = value)) <= 1e-8)
  })
}
