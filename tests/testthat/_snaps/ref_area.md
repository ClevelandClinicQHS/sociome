# non-zcta ref_area works

    Code
      test_tidycensus_data <- eval_tidycensus_calls(partial_tidycensus_calls = test_partial_tidycensus_call,
        geography = "tract", year = 2015, dataset = "acs5", state_county = ref_area_random_geoids$
          state_county, geometry = FALSE, evaluator = purrr::insistently(eval, rate = purrr::rate_delay(),
        quiet = FALSE))
    Message
      
      5 call(s) to tidycensus beginning.
      Getting data from the 2011-2015 5-year ACS
      Getting data from the 2011-2015 5-year ACS
      Getting data from the 2011-2015 5-year ACS
      Getting data from the 2011-2015 5-year ACS
      Getting data from the 2011-2015 5-year ACS

