# invalid or conflicting slots explain how to repair them

    Code
      dr_add_product(dr_workflow(), 1)
    Condition
      Error in `dr_add_product()`:
      ! Supply a dr_product() specification.

# adding an occupied product slot requires update

    Code
      dr_add_product(dr_add_product(dr_workflow(), dr_product("a")), dr_product("b"))
    Condition
      Error in `dr_add_product()`:
      ! This workflow already has a product. Use dr_update_product().

# adding an occupied recipe slot requires update

    Code
      dr_add_recipe(dr_add_recipe(dr_workflow(), dr_recipe()), dr_recipe())
    Condition
      Error in `dr_add_recipe()`:
      ! This workflow already has a recipe. Use dr_update_recipe().

# preparation stays in a dedicated workflow slot

    Code
      dr_add_product(dr_workflow(), dplyr::mutate(dr_product("a"), amount = 1))
    Condition
      Error in `dr_add_product()`:
      ! Keep preparation in a recipe when using dr_add_product().

# sources cannot silently shadow product inputs

    Code
      dr_add_product(dr_add_source(dr_workflow(), data.frame(id = 1L)), dr_product(
        "a", data.frame(id = 2L)))
    Condition
      Error in `dr_add_product()`:
      ! Sources are already set on the product. Supply sources in one place.

# incomplete workflows explain the missing product

    Code
      dr_trial(dr_workflow(), data = data.frame(id = 1L))
    Condition
      Error in `dr_extract_product()`:
      ! This workflow has no product. Use dr_add_product().

