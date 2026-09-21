# engine selection rejects unsupported options

    Code
      dr_set_engine(dr_quality_rule("positive", ~ amount > 0), "spark")
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "native", "pointblank"

# function checks cannot silently become pointblank builders

    Code
      dr_set_engine(dr_quality_rule("positive", function(data) all(data$amount > 0)),
      "pointblank")
    Condition
      Error:
      ! Pointblank formula rules need a one-sided formula. Use dr_pointblank_checks() for an agent builder.

