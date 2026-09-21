#' Define a composable data product
#'
#' Define a data delivery by name, its expected columns and its quality rules.
#' A product is a reusable specification. Bind a source with [dr_add_source()],
#' or supply data when executing a workflow. Use [dr_trial()] before writing.
#'
#' Add named sources, ordinary transformation functions and optional checks or
#' a target. Use [dr_trial()] to try it, or [dr_publish()] to save checked output.
#' [dr_run()] executes the full configuration, including writers. Products can be
#' sources of other products; shared dependencies run once per execution.
#'
#' @section Table models:
#' A table input defines a table product. A dm input defines a model product:
#' its tables and declared relationships are checked together and published in
#' one transaction. dr_collect() returns a dm; select a member explicitly with
#' dr_product("report", result, table = "policies"). Model execution materializes
#' all tables in memory. Table contracts are supplied through contracts.
#'
#' @param id Product identity, unique within a dependency graph.
#' @param data Optional table, path, source adapter, product, or successful run.
#'   Successful lake runs are pinned to their exact published release.
#' @param contract Optional contract, named type vector or prototype list.
#' @param version Optional immutable definition version. When omitted, lake
#'   publication derives a technical version from the definition.
#' @param owner,description Optional metadata, otherwise inherited from contract.
#' @param code_version Optional code and dependency version. Required only when
#'   explicitly reusing a previously published lake release with `cache = TRUE`.
#' @param source_name Optional name for `data`. Defaults to the product ID for
#'   ordinary inputs, or the upstream product ID for a nested product.
#' @param execution Optional connection-free [dr_execution_config()] stored on this
#'   definition. Used when it is the root of [dr_run()], [dr_publish()] or [dataraft.lake::dr_ingest()].
#'   An explicit execution argument overrides these defaults.
#' @param contracts Named table contracts when data is a dm model.
#' @param table Table name to select from a successful model trial or publication.
#'   Published selections retain the exact member release.
#' @returns A `product`, ready for composition, inspection and execution.
#' @export
#' @examples
#' orders <- dr_product("orders", data.frame(id = 1:2, amount = c(25, 75))) |>
#'   dplyr::mutate(amount = round(amount, 2)) |>
#'   dr_add_quality(~ amount >= 0)
#' orders |> dr_trial() |> dr_collect()

dr_product <- function(
  id,
  data = NULL,
  contract = NULL,
  version = "1.0.0",
  owner = NULL,
  description = NULL,
  code_version = NULL,
  source_name = NULL,
  execution = NULL,
  contracts = NULL,
  table = NULL
) {
  x <- new_product(
    id,
    contract,
    version,
    code_version,
    automatic_version = missing(version),
    owner = owner,
    description = description
  )
  execution <- validate_stored_execution(execution)
  if (!is.null(execution)) {
    attr(x, "dr_execution_config") <- execution
  }
  if (!is.null(source_name)) {
    scalar(source_name, "source_name")
  }
  if (is.null(data) && !is.null(source_name)) {
    abort(
      subclass = "dataraft_error_definition",
      "source_name requires data. Name a later source with dr_add_source(name = )."
    )
  }
  if (!is.null(table)) {
    data <- model_member_result(data, table)
  }
  if (inherits(data, "dr_model_result")) {
    abort(
      subclass = "dataraft_error_definition",
      "Choose the model table explicitly with dr_product(..., table = \"name\")."
    )
  }
  if (inherits(data, "dm")) {
    if (!is.null(contract) || !is.null(source_name)) {
      abort(
        subclass = "dataraft_error_definition",
        "For a dm model use contracts = list(table_name = contract)."
      )
    }
    return(new_model_product(x, data, contracts))
  }
  if (!is.null(contracts)) {
    abort(
      subclass = "dataraft_error_definition",
      "contracts is available only for dm model products."
    )
  }
  if (!is.null(data)) {
    source_name <- source_name %||%
      if (inherits(data, "dr_product")) data$id else id
    x <- dr_add_source(x, data, name = source_name)
  }
  x
}


#' Create and validate a relational dm model from pinned releases
#' @param lake Connected lake.
#' @param tables Named character vector of asset ids.
#' @param primary_keys Named list of character vectors, keyed by table alias.
#' @param foreign_keys List of lists containing table, columns, ref_table,
#'   ref_columns.
#' @param releases Optional named vector pinning all table releases.
#' @param check Validate all declared keys and relationships.
#' @return A dm object containing lazy tables. No automatic flattening is done.
#' @export
#' @examplesIf requireNamespace("dataraft.lake", quietly = TRUE) && requireNamespace("duckdb", quietly = TRUE) && requireNamespace("dm", quietly = TRUE)
#' root <- tempfile("dataraft-example-")
#' config <- dataraft.lake::dr_lake_config(
#'   dataraft.lake::dr_registry_duckdb(file.path(root, "lake.db")),
#'   dataraft.lake::dr_storage_local(file.path(root, "data")),
#'   landing = file.path(root, "landing"), backend = "duckdb"
#' )
#' lake <- dataraft.lake::dr_connect_lake(config)
#' path <- file.path(root, "orders.csv")
#' utils::write.csv(data.frame(order_id = 1:2, amount = c(25, 75)), path,
#'   row.names = FALSE)
#' source <- dr_source_file("orders.file", path, reader = utils::read.csv)
#' contract <- dr_contract(
#'   "orders", "1.0.0", "Analytics", "Order amounts", "One order",
#'   c(order_id = "integer", amount = "numeric"), key = "order_id"
#' )
#' release <- dr_product("orders", contract = contract, code_version = "v1") |>
#'   dr_add_source(source) |> dr_publish(to = lake)
#' model <- dr_model(lake, c(orders = "orders"),
#'   primary_keys = list(orders = "order_id"))
#' model
#' dataraft.lake::dr_disconnect_lake(lake)
#' unlink(root, recursive = TRUE)
dr_model <- function(
  lake,
  tables,
  primary_keys = list(),
  foreign_keys = list(),
  releases = NULL,
  check = TRUE
) {
  need("dm")
  need("dataraft.lake", "Models built from published lake releases")
  if (is.null(names(tables)) || anyDuplicated(names(tables))) {
    abort(
      subclass = "dataraft_error_definition",
      "tables must be named uniquely."
    )
  }
  if (!is.null(releases) && !setequal(names(releases), names(tables))) {
    abort(
      subclass = "dataraft_error_definition",
      "Pin all model table releases."
    )
  }
  refs <- lapply(names(tables), function(n) {
    dataraft.lake::resolve_release(
      lake,
      tables[[n]],
      if (is.null(releases)) NULL else releases[[n]]
    )
  })
  names(refs) <- names(tables)
  model <- dm::dm(
    !!!lapply(refs, function(r) {
      dataraft.lake::dr_tbl(lake, r$asset[[1]], r$release_id[[1]])
    })
  )
  model <- dm_keys(model, primary_keys, foreign_keys, check)
  attr(model, "dr_releases") <- lapply(refs, function(r) r$release_id[[1]])
  model
}


#' Extension implementation helper
#'
#' Internal implementation interface for the DataRaft package family.
#' @usage NULL
#' @keywords internal
#' @export
#' @name dm_keys

dm_keys <- function(model, primary_keys, foreign_keys, check) {
  rlang::local_error_call(rlang::caller_env())
  for (name in names(primary_keys)) {
    model <- dm::dm_add_pk(
      model,
      !!rlang::sym(name),
      dplyr::all_of(!!primary_keys[[name]])
    )
  }
  for (fk in foreign_keys) {
    model <- dm::dm_add_fk(
      model,
      !!rlang::sym(fk$table),
      dplyr::all_of(!!fk$columns),
      !!rlang::sym(fk$ref_table),
      dplyr::all_of(!!fk$ref_columns)
    )
  }
  if (check) {
    checks <- dm::dm_examine_constraints(model)
    if (nrow(checks) && !all(checks$is_key)) {
      abort(
        subclass = "dataraft_error_definition",
        "Relational constraints failed.",
        "dr_model_invalid",
        checks = checks
      )
    }
  }
  model
}
