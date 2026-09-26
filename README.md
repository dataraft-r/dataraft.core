# dataraft.core

**Define a data product and check each delivery in R.**

Use this package when the same data arrives again and again and you need to know whether its columns and values still meet your rules. A product brings the input, preparation, contract and checks together; a run returns a result you can inspect. It also works entirely in memory.

[`dataraft` overview](https://github.com/dataraft-r/dataraft) · [Core reference](https://dataraft-r.github.io/dataraft/components/dataraft.core/reference/index.html)

## Try it

```r
library(dataraft.core)

orders <- dr_product("orders", data.frame(id = 1:2, amount = c(10, -2))) |>
  dr_add_contract(c(id = "integer", amount = "numeric")) |>
  dr_add_quality(~ amount >= 0)

result <- dr_run(orders, write = FALSE, stop_on_failure = FALSE)
result$status
dr_quality_rows(result)
```

The negative amount blocks this delivery. Correct the input and run the same product again. `write = FALSE` skips a configured DataRaft writer; source and transformation callbacks may still run.

Install the development package with `pak::pak("dataraft-r/dataraft.core")`. Choose [adapters](https://github.com/dataraft-r/dataraft.adapters) to connect external sources and destinations, or [lake](https://github.com/dataraft-r/dataraft.lake) for versioned releases. The [getting-started guide](https://dataraft-r.github.io/dataraft/articles/get-started.html) walks through the whole flow.
