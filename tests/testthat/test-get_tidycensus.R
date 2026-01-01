test_that("tidycensus_call() works", {
  expect_error(tidycensus_call("get_acs", NULL), "must all be named")
  expect_equal(
    tidycensus_call("get_acs", geography = "state", year = 2010),
    quote(tidycensus::get_acs(geography = "state", year = 2010))
  )
})
