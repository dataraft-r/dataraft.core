# unsafe inheritance and unchanged identity have actionable errors

    Code
      dr_contract_update(old, columns = c(extra = "numeric"))
    Condition
      Error in `dr_contract_update()`:
      ! Change id for a derived contract or version for a revised definition.

---

    Code
      dr_contract_update(old, version = "2", grain = "One month")
    Condition
      Error in `dr_contract_update()`:
      ! Supply key explicitly after changing grain or a key column; use character() for no key.

---

    Code
      dr_contract_update(old, version = "2", grain = "One month", key = character())
    Condition
      Error in `dr_contract_update()`:
      ! Review and supply rules explicitly after removing columns, changing types or changing grain.

---

    Code
      dr_contract_update(old, version = "2", remove = "amount")
    Condition
      Error in `dr_contract_update()`:
      ! Supply required explicitly after removing a required column.

---

    Code
      dr_contract_update(old, version = "2", remove = "amount", required = "id")
    Condition
      Error in `dr_contract_update()`:
      ! Review and supply rules explicitly after removing columns, changing types or changing grain.

---

    Code
      dr_contract_update(old, version = "2", columns = c(id = "character"))
    Condition
      Error in `dr_contract_update()`:
      ! Supply key explicitly after changing grain or a key column; use character() for no key.

---

    Code
      dr_contract_update(old, version = "2", columns = c(amount = "integer"))
    Condition
      Error in `dr_contract_update()`:
      ! Review and supply rules explicitly after removing columns, changing types or changing grain.

---

    Code
      dr_contract_update(old, version = "2", remove = "missing")
    Condition
      Error in `dr_contract_update()`:
      ! remove must name declared contract columns.

---

    Code
      dr_contract_update(old, version = "2", typo = TRUE)
    Condition
      Error in `dr_contract_update()`:
      ! ... must contain unique named contract arguments.

---

    Code
      dr_contract_update(old, version = "2", remove = "amount", columns = c(amount = "numeric"))
    Condition
      Error in `dr_contract_update()`:
      ! A column cannot be both added and removed.

