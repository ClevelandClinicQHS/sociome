context("Test tidycensus-related functions")

test_that("get_adi(), calculate_adi(), areas_in_radius() works", {
  
  expect_equivalent(
    areas_in_radius(
      geography = "state",
      center = lon_lat_from_area(state = "NY", county = "New York"),
      radius = 300,
      units = "km"
    ),
    dplyr::tibble(
      geoid = c("34", "36", "09", "10", "44", "25", "42"),
      longitude = c(-74.432208, -74.620909, -72.870342, -75.556835, -71.450869, -71.36337, -77.00968),
      latitude = c(40.43181, 41.501299, 41.497001, 39.358946, 41.753609, 42.272291, 40.456756),
      distance = c(55.0671408391101, 97.3321317615295, 121.833438895917, 207.925043587838, 236.989536440657, 273.368543029516, 259.974710771119)
    )
  )
  
  expect_equivalent(
    areas_in_radius("tract", center = c(-109.0452, 36.9991), radius = 24),
    dplyr::tibble(
      geoid = c("49037942000", "35045942801", "04001942700", "08083941100"),
      longitude = c(-109.309372, -108.738677, -109.342511, -108.723471),
      latitude = c(37.201652, 36.802506, 36.778617, 37.209925),
      distance = c(20.1996425651449, 21.7243934916106, 22.4131597365958, 22.9596214504155)
    )
  )
  
   # All census tracts within 24 miles of the four corners
   areas_in_radius("tract", center = c(-109.0452, 36.9991), radius = 24)
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  
  load(test_path("nmcounties.rda"))
  
  expect_equivalent(
    get_adi(
      geography = "county",
      state = "NM",
      year = 2017,
      dataset = "acs5",
      keep_indicators = TRUE,
      cache_tables = FALSE
    ),
    nmcounties2017
  )
  
  expect_equivalent(
    get_adi(
      geography = "county",
      state = "NM",
      year = 2010,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    ),
    nmcounties2010
  )
  
  expect_equivalent(
    get_adi(
      geography = "county",
      state = "NM",
      year = 2000,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    ),
    nmcounties2000
  )
  
  skip(
    paste0("1990 decennial data are currently unavailable.",
           "\nWe will reinstate these tests if they are ever restored.")
  )
  
  expect_equivalent(
    get_adi(
      geography = "county",
      state = "NM",
      year = 1990,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    ),
    nmcounties1990
  )
  
})