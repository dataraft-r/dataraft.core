# root slots explicitly replace results while other pinned inputs persist

    Code
      dr_set_sources(root, orders = newer, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Unknown replacement source: orders. Available names: current, historical.

# invalid and overlapping source selectors fail before execution

    Code
      dr_set_sources(root, missing = leaf, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Unknown replacement source: missing. Available names: branch, leaf.

---

    Code
      dr_set_sources(root, leaf = leaf, leaf = leaf, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Supply uniquely named, non-empty replacement sources.

---

    Code
      dr_set_sources(root, leaf, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Supply uniquely named, non-empty replacement sources.

---

    Code
      dr_set_sources(root, branch = dr_product("branch", data.frame(id = 1L)), leaf = leaf,
      .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Overlapping replacements discard requested sources: leaf

---

    Code
      dr_set_sources(ambiguous, leaf = leaf, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Ambiguous root alias and product ID: leaf

---

    Code
      dr_set_sources(root, branch = cyclic, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Product dependency cycle: root -> branch -> root

---

    Code
      dr_set_sources(conflict, branch = branch, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Different product definitions share the ID: leaf

# managed dbt bindings change without reading a database or writing files

    Code
      dr_set_sources(project, orders = corrected, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Ambiguous dbt source binding; use group.table: orders

---

    Code
      dr_set_sources(project, inputs.orders = corrected, historical.orders = NULL,
        .recursive = TRUE)
    Condition
      Error in `FUN()`:
      ! Each source needs a successful immutable lake release, with its asset, release, run and configuration.

---

    Code
      dr_set_sources(changed, unknown = corrected, .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Unknown dbt source binding: unknown. Available names: inputs.orders, inputs.customers, historical.orders.

# correcting a product input retains its transformations and quality gates

    Code
      dr_set_sources(dr_product("root", multiple), multiple = data.frame(id = 2L),
      .recursive = TRUE)
    Condition
      Error in `dr_set_sources()`:
      ! Replacing a product input requires exactly one primary source at `multiple`. Use sources = list(name = value) to select a deeper input or supply an edited product definition.

