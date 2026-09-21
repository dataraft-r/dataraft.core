# formula-engine configuration errors explain the custom-agent escape hatch

    Code
      dr_quality_rule("bad", function(data) TRUE, engine = "pointblank")
    Condition
      Error in `dr_quality_rule()`:
      ! Pointblank formula rules need a one-sided formula. Use dr_pointblank_checks() for an agent builder.

