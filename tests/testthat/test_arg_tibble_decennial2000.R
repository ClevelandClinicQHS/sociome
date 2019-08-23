context("Test get_adi() calls for 2000 decennial")

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
      dataset,
      exec_arg_tibble
    )
  
  tidyr::crossing(exec_arg_tibble, ref_area$state_county)
}


test_geoids <-
  c("01", "11", "39035", "39089", "09001010101", "09001010500", "090159025001")


vars2000 <-
  sociome::decennial_vars %>% 
  dplyr::filter(.data$year == 2000) %>%
  split(.$sumfile) %>% 
  lapply(dplyr::pull, var = "variable")


test_that("call tibble for geoids is correct", {
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "decennial",
      year = 2000,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = vars2000,
      sumfile = c("sf1", "sf3"),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("state"),
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
      year = 2000,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = vars2000,
      sumfile = c("sf1", "sf3"),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("county"),
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
      year = 2000,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = purrr::lmap(vars2000, rep, 4),
      sumfile = c(rep("sf1", 4), rep("sf3", 4)),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("tract"),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = c("01", "11", "39", "09", "01", "11", "39", "09")
    )
  )
  
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "decennial",
      year = 2000,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = purrr::lmap(vars2000, rep, 72),
      sumfile = c(rep("sf1", 72L), rep("sf3", 72L)),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("block group"),
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
    )
  )
})









test_that("call tibble for state only is correct", {

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "decennial",
      year = 2000,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = vars2000,
      sumfile = c("sf1", "sf3"),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("state"),
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
      year = 2000,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = vars2000,
      sumfile = c("sf1", "sf3"),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("county"),
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
      year = 2000,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = purrr::lmap(vars2000, rep, 3L),
      sumfile = c(rep("sf1", 3L), rep("sf3", 3L)),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("tract"),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep(c("de", "dc", "ct"), 2L)
    )
  )
  
  
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")


  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "decennial",
      year = 2000,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = purrr::lmap(vars2000, rep, 12L),
      sumfile = c(rep("sf1", 12L), rep("sf3", 12L)),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("block group"),
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
    )
  )

})




test_that("call tibble for one state and multiple counties is correct", {

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "decennial",
      year = 2000,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = vars2000,
      sumfile = c("sf1", "sf3"),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("state"),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list("oh"),
      county = list(c("cuyahoga", "erie", "lake", "franklin"))
    )
  )

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "county",
      dataset = "decennial",
      year = 2000,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = vars2000,
      sumfile = c("sf1", "sf3"),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("county"),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = list("oh"),
      county = list(c("cuyahoga", "erie", "lake", "franklin"))
    )
  )

  expect_identical(
    test_get_adi_arg_tibble(
      geography = "tract",
      dataset = "decennial",
      year = 2000,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = purrr::lmap(vars2000, rep, 4L),
      sumfile = c(rep("sf1", 4L), rep("sf3", 4L)),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("tract"),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep("oh", 8L),
      county =
        rep(
          c("cuyahoga", "erie", "lake", "franklin"),
          2L
        )
    )
  )


  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "decennial",
      year = 2000,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_decennial),
      variables = purrr::lmap(vars2000, rep, 4L),
      sumfile = c(rep("sf1", 4L), rep("sf3", 4L)),
      year = 2000,
      output = "tidy",
      keep_geo_vars = FALSE,
      geography = list("block group"),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep("oh", 8L),
      county =
        rep(
          c("cuyahoga", "erie", "lake", "franklin"),
          2L
        )
    )
  )

})
