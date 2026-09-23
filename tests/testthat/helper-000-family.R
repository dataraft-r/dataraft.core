# Test bindings for this package; unavailable optional packages are not loaded.
family_owners <- c(
  "dr_capabilities" = "dataraft.core",
  "dr_component_capabilities" = "dataraft.core",
  "dr_catalog_openlineage" = "dataraft.adapters",
  "openlineage_events" = "dataraft.adapters",
  "dr_read_source" = "dataraft.core",
  "dr_source_database" = "dataraft.adapters",
  "dr_execute_transform" = "dataraft.core",
  "product_sources" = "dataraft.core",
  "dr_sql_transform" = "dataraft.adapters",
  "dr_check_component" = "dataraft.core",
  "dr_source_release" = "dataraft.lake",
  "dr_add_source" = "dataraft.core",
  "dr_add_transform" = "dataraft.core",
  "dr_add_contract" = "dataraft.core",
  "dr_add_quality" = "dataraft.core",
  "dr_set_target" = "dataraft.core",
  "dr_add_catalog" = "dataraft.core",
  "dr_inspect" = "dataraft.core",
  "dr_explain" = "dataraft.core",
  "dr_contract_from" = "dataraft.core",
  "infer_column_types" = "dataraft.core",
  "dr_contract_confirm" = "dataraft.core",
  "dr_contract_diff" = "dataraft.core",
  "dr_contract_update" = "dataraft.core",
  "dr_contract" = "dataraft.core",
  "dr_quality_rule" = "dataraft.core",
  "dr_quality_counts" = "dataraft.core",
  "dr_pointblank_checks" = "dataraft.core",
  "quality_row" = "dataraft.core",
  "dr_validate" = "dataraft.core",
  "quality_ok" = "dataraft.core",
  "dr_quality_errors" = "dataraft.core",
  "dr_dbt_sources" = "dataraft.dbt",
  "dr_dbt_project" = "dataraft.dbt",
  "dr_dbt_build" = "dataraft.dbt",
  "dbt_read_artifacts" = "dataraft.dbt",
  "dr_status" = "dataraft.core",
  "dr_quality" = "dataraft.core",
  "dr_releases" = "dataraft.lake",
  "dr_lineage" = "dataraft.core",
  "dr_execution_config" = "dataraft.core",
  "apply_execution_defaults" = "dataraft.core",
  "dr_write_target" = "dataraft.core",
  "dr_write_target.NULL" = "dataraft.core",
  "dr_publish" = "dataraft.core",
  "dr_collect" = "dataraft.core",
  "dr_ingest" = "dataraft.lake",
  "dr_step_precheck" = "dataraft.lake",
  "dr_add_lookup" = "dataraft.core",
  "measurement_set_hash" = "dataraft.metrics",
  "dr_metric_set" = "dataraft.metrics",
  "dr_metric" = "dataraft.metrics",
  "dr_measure" = "dataraft.metrics",
  "dr_as_targets" = "dataraft.adapters",
  "dr_pipeline" = "dataraft.lake",
  "dr_step_land" = "dataraft.lake",
  "dr_step_extract" = "dataraft.lake",
  "dr_step_validate" = "dataraft.lake",
  "dr_step_publish" = "dataraft.lake",
  "run_result" = "dataraft.core",
  "dr_run" = "dataraft.core",
  "dr_add_product" = "dataraft.core",
  "dr_add_recipe" = "dataraft.core",
  "dr_update_product" = "dataraft.core",
  "dr_update_recipe" = "dataraft.core",
  "dr_remove_product" = "dataraft.core",
  "dr_remove_recipe" = "dataraft.core",
  "dr_extract_product" = "dataraft.core",
  "dr_extract_recipe" = "dataraft.core",
  "dr_product" = "dataraft.core",
  "dr_profile_data" = "dataraft.core",
  "dr_run_quality" = "dataraft.core",
  "dr_quality_reference" = "dataraft.core",
  "dr_quality_report" = "dataraft.core",
  "dr_pointblank_report" = "dataraft.core",
  "dr_expect_quality" = "dataraft.core",
  "dr_quality_rows" = "dataraft.core",
  "dr_recipe" = "dataraft.core",
  "dr_step_transform" = "dataraft.core",
  "dr_step_mutate" = "dataraft.core",
  "dr_step_filter" = "dataraft.core",
  "dr_step_select" = "dataraft.core",
  "dr_step_rename" = "dataraft.core",
  "dr_step_arrange" = "dataraft.core",
  "dr_step_summarise" = "dataraft.core",
  "dr_step_distinct" = "dataraft.core",
  "dr_step_lookup" = "dataraft.core",
  "dr_registry" = "dataraft.lake",
  "dr_replace_sources" = "dataraft.core",
  "delivery_aliases" = "dataraft.core",
  "dr_run_history" = "dataraft.core",
  "dr_read_run" = "dataraft.core",
  "dr_incidents" = "dataraft.core",
  "dr_retry_catalogs" = "dataraft.core",
  "safe_run_evidence" = "dataraft.core",
  "save_run_evidence" = "dataraft.core",
  "dr_set_engine" = "dataraft.core",
  "dr_lookup_spec" = "dataraft.core",
  "dr_registry_duckdb" = "dataraft.lake",
  "dr_storage_local" = "dataraft.lake",
  "dr_lake_config" = "dataraft.lake",
  "dr_connect_lake" = "dataraft.lake",
  "dr_disconnect_lake" = "dataraft.lake",
  "dr_open_lake" = "dataraft.lake",
  "dr_close_lake" = "dataraft.lake",
  "dr_write_data" = "dataraft.lake",
  "dr_read_release" = "dataraft.lake",
  "dr_target_lake" = "dataraft.lake",
  "dr_trial" = "dataraft.core",
  "abort" = "dataraft.core",
  "need" = "dataraft.core",
  "scalar" = "dataraft.core",
  "absolute_path" = "dataraft.core",
  "now" = "dataraft.core",
  "canonical" = "dataraft.core",
  "jencode" = "dataraft.core",
  "fingerprint" = "dataraft.core",
  "query" = "dataraft.lake",
  "insert_meta" = "dataraft.lake",
  "dr_workflow" = "dataraft.core",
  "pipeline_step_transform" = "dataraft.lake",
  "dr_plan" = "dataraft.core",
  "with_execution_lake" = "dataraft.lake"
)
for (name in names(family_owners)) {
  owner <- family_owners[[name]]
  if (requireNamespace(owner, quietly = TRUE)) {
    assign(name, get(name, asNamespace(owner), inherits = FALSE))
  }
}
local_family_bindings <- function(..., .package = NULL, .env = parent.frame()) {
  bindings <- list(...)
  if (
    !is.null(.package) && !.package %in% c("dataraft", unique(family_owners))
  ) {
    return(do.call(
      testthat::local_mocked_bindings,
      c(bindings, list(.package = .package, .env = .env))
    ))
  }
  owners <- unname(family_owners[names(bindings)])
  if (anyNA(owners)) {
    stop("Unknown mocked family binding")
  }
  for (owner in unique(owners)) {
    package_bindings <- bindings[owners == owner]
    aliases <- paste0("dr_internal_", names(package_bindings))
    shared <- aliases %in% getNamespaceExports(owner)
    package_bindings <- c(
      package_bindings,
      stats::setNames(package_bindings[shared], aliases[shared])
    )
    do.call(
      testthat::local_mocked_bindings,
      c(package_bindings, list(.package = owner, .env = .env))
    )
  }
}
