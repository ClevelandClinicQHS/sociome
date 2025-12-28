test_that("tidycensus_call() works", {
  expect_error(tidycensus_call("get_acs", NULL), "must all be named")
  expect_equal(
    tidycensus_call("get_acs", geography = "state", year = 2010),
    quote(tidycensus::get_acs(geography = "state", year = 2010))
  )
})



# test_that("getting and filtering tidycensus data works", {
#
#   expect_equal(
#     partial_tidycensus_calls <-
#       make_partial_tidycensus_calls(
#         dataset = 2010
#       )
#   )
#
#   expect_equal(
#     decennial_tracts_2010 <-
#       eval_tidycensus_calls(
#
#       )
#   )
#
#   expect_warning(
#     filter_ref_area()
#   )
# })



