SHOULD_RUN_ASSERTIONS = T

# Testing helper
assert = function(assertion, msg = msg) {
  if(SHOULD_RUN_ASSERTIONS) {
    assert_that(
      assertion,
      msg = msg
    )
  }
}

