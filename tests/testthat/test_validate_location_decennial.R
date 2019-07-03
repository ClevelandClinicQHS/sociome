context("Test validate_location for decennial")

geoid <-
  c("01", "11", "39035", "39089", "09001010101", "09001010500", "090159025001")

test_call <- function(geography) {
  partial_tidycensus_call(
    "decennial",
    2000,
    geography = geography,
    geometry = TRUE,
    shift_geo = TRUE,
    cache_table = TRUE,
    key = NULL,
    moe = 95
  )
}



test_that("validate_location() for decennial - state", {
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = c("de", "ct"),
      county = NULL,
      zcta = NULL,
      geography = "state",
      dataset = "decennial",
      partial_call = test_call("state")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county = list(list(state = c("de", "ct"), county = NULL)),
      zcta = NULL
    )
  )
  
  
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL,
      geography = "state",
      dataset = "decennial",
      partial_call = test_call("state")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county =
        list(
          list(state = "oh", county = c("cuyahoga", "erie", "lake", "franklin"))
        ),
      zcta = NULL
    )
  )
  
  
  
  expect_equal(
    validate_location(
      geoid = geoid,
      state = NULL,
      county = NULL,
      zcta = NULL,
      geography = "state",
      dataset = "decennial",
      partial_call = test_call("state")
    ),
    list(
      geoid = geoid,
      geo_length = 2,
      state_county =
        list(list(state = c("01", "11", "39", "09"), county = NULL)),
      zcta = NULL
    )
  )
})





test_that("validate_location() for decennial - county", {  
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = c("de", "ct"),
      county = NULL,
      zcta = NULL,
      geography = "county",
      dataset = "decennial",
      partial_call = test_call("county")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county = 
        list(list(state = c("de", "ct"), county = NULL)),
      zcta = NULL
    )
  )
  
  
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL,
      geography = "county",
      dataset = "decennial",
      partial_call = test_call("county")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county =
        list(
          list(state = "oh", county = c("cuyahoga", "erie", "lake", "franklin"))
        ),
      zcta = NULL
    )
  )
  
  
  
  expect_equal(
    validate_location(
      geoid = geoid,
      state = NULL,
      county = NULL,
      zcta = NULL,
      geography = "county",
      dataset = "decennial",
      partial_call = test_call("county")
    ),
    list(
      geoid = geoid,
      geo_length = 5,
      state_county = 
        list(list(state = c("01", "11", "39", "09"), county = NULL)),
      zcta = NULL
    )
  )
})




test_that("validate_location() for decennial - tract", {  
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = c("de", "ct"),
      county = NULL,
      zcta = NULL,
      geography = "tract",
      dataset = "decennial",
      partial_call = test_call("tract")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county = 
        list(
          list(state = "de", county = NULL),
          list(state = "ct", county = NULL)
        ),
      zcta = NULL
    )
  )
  
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL,
      geography = "tract",
      dataset = "decennial",
      partial_call = test_call("tract")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county =
        list(
          list(state = "oh", county = "cuyahoga"),
          list(state = "oh", county = "erie"),
          list(state = "oh", county = "lake"),
          list(state = "oh", county = "franklin")
        ),
      zcta = NULL
    )
  )
  
  
  expect_equal(
    validate_location(
      geoid = geoid,
      state = NULL,
      county = NULL,
      zcta = NULL,
      geography = "tract",
      dataset = "decennial",
      partial_call = test_call("tract")
    ),
    list(
      geoid = geoid,
      geo_length = 11,
      state_county = 
        list(
          list(state = "01", county = NULL),
          list(state = "11", county = NULL),
          list(state = "39", county = NULL),
          list(state = "09", county = NULL)
        ),
      zcta = NULL
    )
  )
})


test_that("validate_location() for decennial - block group", {  
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = "oh",
      county = c("cuyahoga", "erie", "lake", "franklin"),
      zcta = NULL,
      geography = "block group",
      dataset = "decennial",
      partial_call = test_call("block group")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county =
        list(
          list(state = "oh", county = "cuyahoga"),
          list(state = "oh", county = "erie"),
          list(state = "oh", county = "lake"),
          list(state = "oh", county = "franklin")
        ),
      zcta = NULL
    )
  )
  
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "", "No CENSUS_API_KEY in .Renviron")
  
  expect_equal(
    validate_location(
      geoid = NULL,
      state = c("de", "ct"),
      county = NULL,
      zcta = NULL,
      geography = "block group",
      dataset = "decennial",
      partial_call = test_call("block group")
    ),
    list(
      geoid = NULL,
      geo_length = NULL,
      state_county = 
        list(
          list(state = "09", county = "001"),
          list(state = "09", county = "003"),
          list(state = "09", county = "005"),
          list(state = "09", county = "007"),
          list(state = "09", county = "009"),
          list(state = "09", county = "011"),
          list(state = "09", county = "013"),
          list(state = "09", county = "015"),
          list(state = "10", county = "001"),
          list(state = "10", county = "003"),
          list(state = "10", county = "005")
        ),
      zcta = NULL
    )
  )
  
  
  
  expect_equal(
    validate_location(
      geoid = geoid,
      state = NULL,
      county = NULL,
      zcta = NULL,
      geography = "block group",
      dataset = "decennial",
      partial_call = test_call("block group")
    ),
    list(
      geoid = geoid,
      geo_length = 12,
      state_county = 
        list(
          list(state = "39", county = "035"),
          list(state = "39", county = "089"),
          list(state = "09", county = "001"),
          list(state = "09", county = "015"),
          
          list(state = "01", county = "001"),
          list(state = "01", county = "003"),
          list(state = "01", county = "005"),
          list(state = "01", county = "007"),
          list(state = "01", county = "009"),
          list(state = "01", county = "011"),
          list(state = "01", county = "013"),
          list(state = "01", county = "015"),
          list(state = "01", county = "017"),
          list(state = "01", county = "019"),
          list(state = "01", county = "021"),
          list(state = "01", county = "023"),
          list(state = "01", county = "025"),
          list(state = "01", county = "027"),
          list(state = "01", county = "029"),
          list(state = "01", county = "031"),
          list(state = "01", county = "033"),
          list(state = "01", county = "035"),
          list(state = "01", county = "037"),
          list(state = "01", county = "039"),
          list(state = "01", county = "041"),
          list(state = "01", county = "043"),
          list(state = "01", county = "045"),
          list(state = "01", county = "047"),
          list(state = "01", county = "049"),
          list(state = "01", county = "051"),
          list(state = "01", county = "053"),
          list(state = "01", county = "055"),
          list(state = "01", county = "057"),
          list(state = "01", county = "059"),
          list(state = "01", county = "061"),
          list(state = "01", county = "063"),
          list(state = "01", county = "065"),
          list(state = "01", county = "067"),
          list(state = "01", county = "069"),
          list(state = "01", county = "071"),
          list(state = "01", county = "073"),
          list(state = "01", county = "075"),
          list(state = "01", county = "077"),
          list(state = "01", county = "079"),
          list(state = "01", county = "081"),
          list(state = "01", county = "083"),
          list(state = "01", county = "085"),
          list(state = "01", county = "087"),
          list(state = "01", county = "089"),
          list(state = "01", county = "091"),
          list(state = "01", county = "093"),
          list(state = "01", county = "095"),
          list(state = "01", county = "097"),
          list(state = "01", county = "099"),
          list(state = "01", county = "101"),
          list(state = "01", county = "103"),
          list(state = "01", county = "105"),
          list(state = "01", county = "107"),
          list(state = "01", county = "109"),
          list(state = "01", county = "111"),
          list(state = "01", county = "113"),
          list(state = "01", county = "115"),
          list(state = "01", county = "117"),
          list(state = "01", county = "119"),
          list(state = "01", county = "121"),
          list(state = "01", county = "123"),
          list(state = "01", county = "125"),
          list(state = "01", county = "127"),
          list(state = "01", county = "129"),
          list(state = "01", county = "131"),
          list(state = "01", county = "133"),
          
          list(state = "11", county = "001")
        ),
      zcta = NULL
    )
  )
})
