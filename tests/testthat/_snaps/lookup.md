# lookup key errors explain the equality-only boundary

    Code
      dr_add_lookup(dr_product("orders"), data.frame(id = 1L), by = dplyr::join_by(
        id > id))
    Condition
      Error in `dr_add_lookup()`:
      ! A checked lookup needs equality keys. Use an ordinary dplyr join for other relationships.

