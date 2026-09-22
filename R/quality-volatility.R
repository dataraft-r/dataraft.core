# A conservative guard for recognizable volatile calls, not a purity proof.
# Untrusted arbitrary R callbacks still run with the caller's permissions.
quality_volatile_calls <- function(check, seen = list(), depth = 0L) {
  if (depth > 30L) return("unresolved callback depth")
  if (is.function(check)) {
    if (is.primitive(check) || any(vapply(seen, identical, logical(1), check))) {
      return(character())
    }
    seen <- c(seen, list(check))
    expression <- body(check)
    env <- environment(check)
  } else if (inherits(check, "formula")) {
    expression <- check[[2L]]
    env <- environment(check)
  } else {
    return(character())
  }
  known <- c(
    "Sys.time", "Sys.Date", "proc.time", "date", "sample", "sample.int",
    "runif", "rnorm", "rbinom", "rpois", "rexp", "rgamma", "rbeta",
    "rchisq", "rt", "rf", "rcauchy", "rgeom", "rhyper", "rlnorm",
    "rlogis", "rmultinom", "rnbinom", "rweibull", "RANDOM", "RAND",
    "NOW", "CURRENT_TIMESTAMP", "CURRENT_DATE"
  )
  walk <- function(node) {
    if (!is.call(node)) return(character())
    head <- node[[1L]]
    name <- if (is.symbol(head)) as.character(head) else ""
    if (is.call(head) && is.symbol(head[[1L]]) &&
        as.character(head[[1L]]) %in% c("::", ":::")) {
      name <- as.character(head[[3L]])
    }
    found <- if (name %in% known) name else character()
    if (identical(name, "sql") && length(node) > 1L && is.character(node[[2L]])) {
      if (grepl("\\b(RANDOM|RAND|NOW|CURRENT_TIMESTAMP|CURRENT_DATE)\\b", node[[2L]], ignore.case = TRUE)) {
        found <- c(found, "volatile SQL")
      }
    }
    # Inspect user-bound helper functions, but do not traverse entire namespaces.
    if (is.symbol(head) && nzchar(name) && exists(name, env, inherits = TRUE)) {
      owner <- binding_environment(name, env)
      if (bindingIsActive(name, owner) ||
          isTRUE(rlang::env_binding_are_lazy(owner, name)[[1L]])) {
        found <- c(found, "unresolved callback binding")
      } else {
        fn <- get(name, env, inherits = TRUE)
        if (is.function(fn)) {
          aliases <- vapply(known, function(candidate) {
            original <- get0(candidate, envir = asNamespace("stats"), inherits = TRUE)
            is.function(original) && identical(fn, original)
          }, logical(1))
          found <- c(found, known[aliases])
        }
        if (is.function(fn) && !is.primitive(fn) &&
            !isNamespace(environment(fn)) && !identical(environment(fn), baseenv())) {
          found <- c(found, quality_volatile_calls(fn, seen, depth + 1L))
        }
      }
    }
    unique(c(found, unlist(lapply(as.list(node)[-1L], walk), use.names = FALSE)))
  }
  walk(expression)
}

binding_environment <- function(name, env) {
  while (!exists(name, env, inherits = FALSE)) env <- parent.env(env)
  env
}

assert_quality_volatility <- function(rule) {
  if (isTRUE(rule$volatile)) return(invisible(rule))
  calls <- quality_volatile_calls(rule$check)
  if (length(calls)) {
    abort(
      "This quality rule uses changing state. Declare volatile = TRUE for diagnostic evaluation only.",
      subclass = "dataraft_error_quality_volatile"
    )
  }
  invisible(rule)
}

volatile_quality_evidence <- function(evidence, rule) {
  if (isTRUE(rule$volatile)) {
    evidence$status[evidence$status %in% c("passed", "warning")] <- "unvalidated"
    evidence$message <- "Volatile rule: diagnostic evidence only; publication and approval are disabled."
  }
  evidence
}
