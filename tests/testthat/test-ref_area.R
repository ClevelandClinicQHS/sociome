test_that("zcta ref_area works", {
  # Setup
  test_partial_tidycensus_calls <-
    list(
      quote(
        tidycensus::get_acs(
          geography = "tract",
          year = 2010,
          survey = "acs5",
          variables = "B01001_001",
          table = NULL,
          cache_table = TRUE,
          output = "tidy",
          keep_geo_vars = FALSE,
          summary_var = NULL)
      )
    )
  evaluator <-
    purrr::insistently(eval, rate = purrr::rate_delay(), quiet = FALSE)

  expect_error(
    ref_area(
      geography = "zip code tabulation area",
      zcta = "44136",
      state = "oh",
      county = "lake",
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    "county must be NULL"
  )
  expect_error(
    ref_area(
      geography = "zip code tabulation area",
      zcta = "44136",
      state = "oh",
      county = NULL,
      geoid = "39035",
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    "geoid must be NULL"
  )
  expect_error(
    ref_area(
      geography = "zip code tabulation area",
      zcta = character(),
      state = "oh",
      county = NULL,
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    "be a character vector of digits"
  )
  expect_error(
    ref_area(
      geography = "zip code tabulation area",
      zcta = c(NA, " 44136"),
      state = "oh",
      county = NULL,
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    "be a character vector of digits"
  )
  expect_error(
    ref_area(
      geography = "zip code tabulation area",
      zcta = c(" 44136  ", "foo "),
      state = "oh",
      county = NULL,
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    "be a character vector of digits"
  )
  expect_equal(
    ref_area(
      geography = "zip code tabulation area",
      zcta = c(" 44136  ", "44147 "),
      state = "oh",
      county = NULL,
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    list(
      geoid        = NULL,
      geo_length   = NULL,
      state_county = dplyr::tibble(state = list("oh"), county = list(NULL)),
      zcta         = c("44136", "44147")
    )
  )
})



test_that("non-zcta ref_area works", {
  # Setup
  test_partial_tidycensus_calls <-
    list(
      quote(
        tidycensus::get_acs(
          geography = "tract",
          year = 2010,
          survey = "acs5",
          variables = "B01001_001",
          table = NULL,
          cache_table = TRUE,
          output = "tidy",
          keep_geo_vars = FALSE,
          summary_var = NULL)
      )
    )
  evaluator <-
    purrr::insistently(eval, rate = purrr::rate_delay(), quiet = FALSE)

  # State/county

  expect_message(
    counties_from_state <-
      county_geoids_from_state(
        partial_tidycensus_calls = test_partial_tidycensus_calls,
        state = "de",
        evaluator = evaluator
      ),
    "Preliminary"
  )

  expect_equal(
    counties_from_state,
    c("10001", "10003", "10005")
  )

  expect_equal(
    sc_from_county_geoids(c("10001", "10003", "10005")),
    dplyr::tibble(state = "10", county = c("001", "003", "005"))
  )

  expect_message(
    sc_from_delaware <-
      sc_from_preliminary_call(
        partial_tidycensus_calls = test_partial_tidycensus_calls,
        state = "de",
        evaluator = evaluator
      ),
    "Preliminary"
  )

  expect_equal(
    sc_from_delaware,
    dplyr::tibble(state = "10", county = c("001", "003", "005"))
  )

  expect_error(
    ref_area(
      geography = "county",
      zcta = NULL,
      state = c("oh", "pa"),
      county = c("lake", "licking"),
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    "If supplying counties, exactly one state must be provided"
  )
  expect_equal(
    ref_area(
      geography = "county",
      zcta = NULL,
      state = "oh",
      county = c("lake", "licking"),
      geoid = NULL,
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county = dplyr::tibble(state = list("oh"), county = list(c("lake", "licking"))),
      zcta = NULL
    )
  )
  expect_equal(
    ref_area(
      geography = "tract",
      zcta = NULL,
      state = NULL,
      county = NULL,
      geoid = NULL,
      dataset = "decennial",
      year = 2000,
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county = dplyr::tibble(state = state_geoids),
      zcta = NULL
    )
  )

  expect_message(
    granular_ref_area_from_delware <-
      ref_area(
        geography = "tract",
        zcta = NULL,
        state = "de",
        county = NULL,
        geoid = NULL,
        year = 2010,
        dataset = "acs5",
        partial_tidycensus_calls = test_partial_tidycensus_calls,
        evaluator = evaluator
      )
  )

  expect_equal(
    granular_ref_area_from_delware,
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county =
        dplyr::tibble(state = "10", county = c("001", "003", "005")),
      zcta = NULL
    )
  )

  expect_equal(
    ref_area(
      geography = "tract",
      zcta = NULL,
      state = "oh",
      county = c(" cuyahoga  ", "licking "),
      geoid = NULL,
      dataset = "decennial",
      year = 2000,
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county =
        dplyr::tibble(state = "oh", county = c(" cuyahoga  ", "licking ")),
      zcta = NULL
    )
  )

  # GEOIDs
  expect_equal(
    sc_from_geoid(
      geoid = c("10001", "10003", "39"),
      geography = "state",
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    dplyr::tibble(state = list(c("10", "39")))
  )
  expect_equal(
    sc_from_geoid(
      geoid = c("10001", "10003", "39"),
      geography = "tract",
      year = 2000,
      dataset = "decennial",
      partial_tidycensus_calls = test_partial_tidycensus_calls,
      evaluator = evaluator
    ),
    dplyr::tibble(state = c("10", "39"))
  )
  expect_message(
    sc_from_random_geoids <-
      sc_from_geoid(
        geoid = c("10", "39035136101", "390351513001", "01001"),
        geography = "tract",
        year = 2015,
        dataset = "acs1",
        partial_tidycensus_calls = test_partial_tidycensus_calls,
        evaluator = evaluator
      ),
    "Preliminary"
  )
  expect_equal(
    sc_from_random_geoids,
    dplyr::tibble(
      state = c("39", "01", "10", "10", "10"),
      county = c("035", "001", "001", "003", "005")
    )
  )

  expect_warning(
    suppressMessages(
      ref_area_random_geoids <-
        ref_area(
          geography = "tract",
          state = NULL,
          county = NULL,
          zcta = NULL,
          geoid =
            c("10", "39035136101", "390351513001", "390356942069", "01001"),
          year = 2015,
          dataset = "acs5",
          partial_tidycensus_calls = test_partial_tidycensus_calls,
          evaluator = evaluator
        )
    ),
    "One or more geoids are more granular than geography"
  )

  expect_equal(
    ref_area_random_geoids,
    list(
      geoid = c("10", "39035136101", "390351513001", "390356942069", "01001"),
      geo_length = 11L,
      state_county =
        dplyr::tibble(
          state = c("39", "01", "10", "10", "10"),
          county = c("035", "001", "001", "003", "005")
        ),
      zcta = NULL
    )
  )

  expect_message(
    suppressWarnings(
      ref_area(
        geography = "tract",
        state = NULL,
        county = NULL,
        zcta = NULL,
        geoid = c("10", "39035136101", "390351513001", "01001"),
        year = 2015,
        dataset = "acs5",
        partial_tidycensus_calls = test_partial_tidycensus_calls,
        evaluator = evaluator
      )
    ),
    "Preliminary"
  )

  expect_equal(
    test_partial_tidycensus_call <-
      make_partial_tidycensus_calls(
        dataset = "acs5",
        year = 2015,
        geography = "tract",
        cache_tables = FALSE,
        geometry = FALSE,
        key = NULL
      ),
    rlang::exprs(
      get_acs =
        tidycensus::get_acs(
          geography = "tract",
          variables =
            !!sociome::acs_vars[
              sociome::acs_vars$set1,
              "variable",
              drop = TRUE
            ],
          table = NULL,
          cache_table = FALSE,
          year = 2015,
          output = "tidy",
          geometry = FALSE,
          keep_geo_vars = FALSE,
          summary_var = NULL,
          key = NULL,
          survey = "acs5"
        )
    )
  )

  expect_snapshot(
    test_tidycensus_data <-
      eval_tidycensus_calls(
        partial_tidycensus_calls = test_partial_tidycensus_call,
        geography = "tract",
        year = 2015,
        dataset = "acs5",
        state_county = ref_area_random_geoids$state_county,
        geometry = FALSE,
        evaluator =
          purrr::insistently(eval, rate = purrr::rate_delay(), quiet = FALSE)
      )
  )

  expect_warning(
    test_tidycensus_data <-
      filter_ref_area(
        d = test_tidycensus_data,
        what = "tract",
        pattern = c("10", "39035136101", "390351513001", "390356942069", "01001"),
        geo_length = 11L
      ),
    "The following tracts had no match in census data"
  )


  expect_error(calculate_adi(NULL), "data must be a tibble")
  expect_warning(calculate_adi(test_tidycensus_data[1:29, ]), "fewer than 30")

  expect_error(
    get_adi("tract", state = "ny", county = "new york", year = 2015),
    "Imputation unsuccessful"
  )
})
