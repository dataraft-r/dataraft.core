# definition, inspection and validation never call user functions

    Code
      print(product)
    Output
      <Data product: orders >
      Deliveries: source_1
      Transformations: 1
      Contract: automatic structure
      Quality: 0 rules
      Target: memory
      Status: defined
    Code
      dr_explain(product)
    Output
      Product: orders
      Read: 1 named source(s).
      Deliveries: source_1.
      Replace a delivery with sources = list(delivery_name = new_data).
      Transforms receive one table, which may stay lazy.
      Transform: 1 ordered step(s).
      Check: inferred structure and 0 additional rule(s).
      Return: checked data and run evidence. dr_collect() materializes lazy output.
      Materialization: lazy tables remain lazy unless a component collects.
      dr_validate() checks configuration and dependency cycles; dr_run() executes.

# invalid specifications fail before acquisition

    Code
      dr_validate(dr_product("orders"))
    Condition
      Error in `dr_validate()`:
      ! This product has no source. Add one with dr_add_source().

# bad transformations preserve an actionable condition and run evidence

    Code
      dr_collect(result)
    Condition
      Error in `dplyr::collect()`:
      ! orders failed during execution; no successful output is available.
      i Use result <- dr_last_failure(), then inspect dr_quality_report(result) and dr_quality_rows(result).
      i Inspect dr_quality_errors(result) for locally retained rule exceptions.
      Caused by error:
      ! An execution error was retained in condition$result$error for local inspection.

# contract prototypes, anonymous contracts and rule names normalize consistently

    Code
      dr_contract(columns = c(id = "integer", id = "numeric"))
    Condition
      Error in `dr_contract()`:
      ! columns must be a named type vector.

# duplicate rule names across a contract and added checks fail preflight

    Code
      dr_validate(product)
    Condition
      Error in `dr_validate()`:
      ! Contract and added quality rules must have unique names.

# catalog delivery gets metadata without data rows or connections

    Code
      result <- dr_run(product)
    Condition
      Warning:
      Catalog `callback-1` delivery failed; execution status is unchanged. Inspect result$catalog_delivery and retry with fresh metadata.
    Code
      print(result$status)
    Output
      [1] "completed"
    Code
      print(result$warnings)
    Output
      [1] "Catalog `callback-1` delivery failed; execution status is unchanged. Inspect result$catalog_delivery and retry with fresh metadata."

# execution warnings remain inspectable without entering metadata text

    Code
      result <- dr_run(product)
    Condition
      Warning:
      Execution produced 1 warning(s); inspect result$warning_conditions locally.
    Code
      print(result$warnings)
    Output
      [1] "Execution produced 1 warning(s); inspect result$warning_conditions locally."

