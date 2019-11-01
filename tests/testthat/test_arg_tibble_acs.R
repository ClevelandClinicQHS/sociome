context("Test get_adi() calls for acs")

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


testvars_acs2014 <- c("B11005_001",
                      "B15003_001", "B15003_002", "B15003_003", "B15003_004",
                      "B15003_005", "B15003_006", "B15003_007", "B15003_008",
                      "B15003_009", "B15003_010", "B15003_011", "B15003_012",
                      "B15003_017", "B15003_018", "B15003_019", "B15003_020",
                      "B15003_021", "B15003_022", "B15003_023", "B15003_024",
                      "B15003_025", "B17010_001", "B17010_002", "B19001_002",
                      "B19001_011", "B19001_012", "B19001_013", "B19001_014",
                      "B19001_015", "B19001_016", "B19001_017", "B19113_001",
                      "B11005_002", "B11005_005", "B23025_003", "B23025_005",
                      "B25003_001", "B25003_002", "B25014_001", "B25014_005",
                      "B25014_006", "B25014_007", "B25014_011", "B25014_012",
                      "B25014_013", "B25044_001", "B25044_003", "B25044_010",
                      "B25064_001", "B25077_001", "B25088_002", "C17002_001",
                      "C17002_002", "C17002_003", "C17002_004", "C17002_005",
                      "C24010_001", "C24010_003", "C24010_039")

test_geoids <-
  c("01", "11", "39035", "39089", "09001010101", "09001010500", "090159025001")


test_that("call tibble for geoids is correct", {
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "acs5",
      year = 2014,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "state",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
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
      dataset = "acs5",
      year = 2014,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "county",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
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
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "tract",
      dataset = "acs5",
      year = 2014,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "tract",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = c(rep("01", 67L), "09", "09", "11", "39", "39"),
      county =
        c("001", "003", "005", "007", "009", "011", "013", "015", "017", "019", "021", "023", "025", "027", "029", "031", "033", "035", "037", "039", "041", "043", "045", "047", "049", "051", "053", "055", "057", "059", "061", "063", "065", "067", "069", "071", "073", "075", "077", "079", "081", "083", "085", "087", "089", "091", "093", "095", "097", "099", "101", "103", "105", "107", "109", "111", "113", "115", "117", "119", "121", "123", "125", "127", "129", "131", "133",
          "001", "015",
          "001",
          "035", "089")
    )
  )
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "acs5",
      year = 2014,
      geoid = test_geoids,
      state = NULL,
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "block group",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = c(rep("01", 67L), "09", "09", "11", "39", "39"),
      county =
        c("001", "003", "005", "007", "009", "011", "013", "015", "017", "019", "021", "023", "025", "027", "029", "031", "033", "035", "037", "039", "041", "043", "045", "047", "049", "051", "053", "055", "057", "059", "061", "063", "065", "067", "069", "071", "073", "075", "077", "079", "081", "083", "085", "087", "089", "091", "093", "095", "097", "099", "101", "103", "105", "107", "109", "111", "113", "115", "117", "119", "121", "123", "125", "127", "129", "131", "133",
          "001", "015",
          "001",
          "035", "089")
    )
  )
})




test_that("call tibble for states only is correct", {
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "state",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
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
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "county",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
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
  
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "tract",
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "tract",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = c("09", "09", "09", "09", "09", "09", "09", "09", "10", "10", "10", "11"),
      county = c("001", "003", "005", "007", "009", "011", "013", "015", "001", "003", "005", "001")
    )
  )
  
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = c("de", "dc", "ct"),
      county = NULL,
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "block group",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = c("09", "09", "09", "09", "09", "09", "09", "09", "10", "10", "10", "11"),
      county = c("001", "003", "005", "007", "009", "011", "013", "015", "001", "003", "005", "001")
    )
  )
  
})




test_that("call tibble for one state and multiple counties is correct", {
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "state",
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "state",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
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
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "county",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
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
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "tract",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep("oh", 4),
      county = c("cuyahoga", "erie", "franklin", "lake")
    )
  )
  
  
  expect_identical(
    test_get_adi_arg_tibble(
      geography = "block group",
      dataset = "acs5",
      year = 2014,
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "franklin", "lake"),
      zcta = NULL
    ),
    tibble::tibble(
      .fn = list(tidycensus::get_acs),
      geography = "block group",
      variables = list(testvars_acs2014),
      year = 2014,
      survey = "acs5",
      output = "tidy",
      keep_geo_vars = FALSE,
      endyear = list(NULL),
      geometry = list(FALSE),
      shift_geo = list(FALSE),
      cache_table = list(FALSE),
      key = list(NULL),
      state = rep("oh", 4),
      county = c("cuyahoga", "erie", "franklin", "lake"),
    )
  )
  
})
