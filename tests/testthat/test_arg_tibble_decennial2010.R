context("Test get_adi() calls for 2010 decennial")

test_get_adi_arg_tibble <- function(geography,
                                    dataset,
                                    year,
                                    geoid,
                                    state,
                                    county,
                                    zcta,
                                    ...) {
  exec_arg_tibble <-
    exec_arg_tibble(
      dataset,
      year, 
      geography = geography,
      geometry = FALSE,
      shift_geo = FALSE,
      cache_table = FALSE,
      key = NULL,
      ...
    )
  
  ref_area <-
    validate_location(
      geoid, 
      state, 
      county, 
      zcta, 
      geography, 
      year,
      dataset,
      exec_arg_tibble
    )
  
  cross_args(exec_arg_tibble, ref_area$state_county, geography, year, dataset)
}


test_geoids <-
  c("01", "11", "39035", "39089", "09001010101", "09001010500", "090159025001")


vars2010 <-
  sociome::decennial_vars %>% 
  dplyr::filter(.data$year == 2010) %>%
  dplyr::pull("variable") %>% 
  list(
    sociome::acs_vars %>%
      dplyr::filter(.data$dec2010) %>%
      dplyr::pull("variable")
  )


test_that("call tibble for geoids is correct", {
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "decennial",
      year = 2010,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
      geography = "state",
      variables = vars2010,
      sumfile = list("sf1", NULL),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list(c("01", "11", "39", "09"))
    )
  )
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "county",
      dataset = "decennial",
      year = 2010,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
      geography = "county",
      variables = vars2010,
      sumfile = list("sf1", NULL),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list(c("01", "11", "39", "09"))
    )
  )
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "tract",
      dataset = "decennial",
      year = 2010,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = c(rep(list(tidycensus::get_decennial), 4L), rep(list(tidycensus::get_acs), 72)),
      geography = "tract",
      variables = c(rep(vars2010[1L], 4L), rep(vars2010[2L], 72)),
      sumfile = c(rep("sf1", 4L), rep(list(NULL), 72L)),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = c("39", "09", "01", "11",
                "39", "39", "09", "09", rep("01", 67L), "11"),
      county =
        c(rep(list(NULL), 4L),
          "035", "089",
          "001", "015",
          "001", "007", "017", "019", "025", "013", "009", "011", "021", "023", "037", "027", "029", "031", "035", "033", "053", "039", "041", "043", "045", "047", "049", "051", "065", "055", "057", "059", "061", "063", "081", "067", "069", "071", "073", "075", "077", "079", "099", "083", "085", "087", "089", "091", "093", "097", "095", "115", "101", "103", "105", "107", "109", "111", "113", "127", "117", "119", "121", "123", "125", "129", "131", "133", "003", "005", "015",
          "001")
    )
  )
  
  
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  
  
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "decennial",
      year = 2010,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ) %>%
      dplyr::arrange(.data$state, .data$county),
    tibble::tibble(
      .fn = c(rep(list(tidycensus::get_decennial), 72L), rep(list(tidycensus::get_acs), 72L)),
      geography = "block group",
      variables = purrr::lmap(vars2010, rep, 72),
      sumfile = c(rep("sf1", 72L), rep(list(NULL), 72L)),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state =
        rep(
          c("39", "39", "09", "09", rep("01", 67L), "11"),
          2L
        ),
      county =
        rep(
          c("035", "089",
            "001", "015",
            "001", "003", "005", "007", "009", "011", "013", "015", "017", "019", "021", "023", "025", "027", "029", "031", "033", "035", "037", "039", "041", "043", "045", "047", "049", "051", "053", "055", "057", "059", "061", "063", "065", "067", "069", "071", "073", "075", "077", "079", "081", "083", "085", "087", "089", "091", "093", "095", "097", "099", "101", "103", "105", "107", "109", "111", "113", "115", "117", "119", "121", "123", "125", "127", "129", "131", "133",
            "001"),
          2L
        )
    ) %>%
      dplyr::arrange(.data$state, .data$county)
  )
})









test_that("call tibble for state only is correct", {

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
      geography = "state",
      variables = vars2010,
      sumfile = list("sf1", NULL),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list(c("de", "dc", "ct")),
      county = list(NULL)
    )
  )

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "county",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
      geography = "county",
      variables = vars2010,
      sumfile = list("sf1", NULL),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list(c("de", "dc", "ct")),
      county = list(NULL)
    )
  )

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "tract",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = c(rep(list(tidycensus::get_decennial), 3L),
              rep(list(tidycensus::get_acs), 12)),
      geography = "tract",
      variables = c(rep(vars2010[1L], 3L), rep(vars2010[2L], 12L)),
      sumfile = c(rep("sf1", 3L), rep(list(NULL), 12L)),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = 
        c("09", "10", "11",
          "09", "09", "09", "09", "09", "09", "09", "09", "10", "10", "10", "11"),
      county = 
        c(rep(list(NULL), 3L),
          "001", "005", "003", "007", "015", "009", "011", "013", "001", "005", "003", "001")
    )
  )
  
  
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")


  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ) %>% dplyr::arrange(.data$state, .data$county),
    tibble::tibble(
      .fn = c(rep(list(tidycensus::get_decennial), 12L), rep(list(tidycensus::get_acs), 12L)),
      geography = "block group",
      variables = purrr::lmap(vars2010, rep, 12L),
      sumfile = c(rep("sf1", 12L), rep(list(NULL), 12L)),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = 
        rep(
          c("09", "09", "09", "09", "09", "09", "09", "09", "10", "10", "10", "11"),
          2L
        ),
      county =
        rep(
          c("001", "003", "005", "007", "009", "011", "013", "015", "001", "003", "005", "001"),
          2L
        )
    ) %>% 
      dplyr::arrange(.data$state, .data$county)
  )

})




test_that("call tibble for one state and multiple counties is correct", {

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
      geography = "state",
      variables = vars2010,
      sumfile = list("sf1", NULL),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list("oh"),
      county = list(c("cuyahoga", "erie", "franklin", "lake"))
    )
  )

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "county",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
      geography = "county",
      variables = vars2010,
      sumfile = list("sf1", NULL),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list("oh"),
      county = list(c("cuyahoga", "erie", "franklin", "lake"))
    )
  )

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "tract",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = c(tidycensus::get_decennial, rep(list(tidycensus::get_acs), 4L)),
      geography = "tract",
      variables = c(vars2010[1L], rep(vars2010[2L], 4L)),
      sumfile = c("sf1", rep(list(NULL), 4L)),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep("oh", 5L),
      county = list(NULL, "cuyahoga", "erie", "franklin", "lake")
    )
  )


  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "decennial",
      year = 2010,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = c(rep(list(tidycensus::get_decennial), 4L), rep(list(tidycensus::get_acs), 4L)),
      geography = "block group",
      variables = purrr::lmap(vars2010, rep, 4L),
      sumfile = c(rep("sf1", 4L), rep(list(NULL), 4L)),
      year = 2010,
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep("oh", 8L),
      county =
        c("cuyahoga", "erie", "franklin", "lake",
          "cuyahoga", "erie", "franklin", "lake")
    )
  )

})
