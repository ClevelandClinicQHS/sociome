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
      STATEFP = c("34", "36", "09", "10", "44", "42", "25"),
      STNAME = c("New Jersey", "New York", "Connecticut", "Delaware", "Rhode Island", "Pennsylvania", "Massachusetts"),
      POPULATION = c(8791894L, 19378102L, 3574097L, 897934L, 1052567L, 12702379L, 6547629L), 
      LATITUDE = c(40.43181, 41.501299, 41.497001, 39.358946, 41.753609, 40.456756, 42.272291),
      LONGITUDE = c(-74.432208, -74.620909, -72.870342, -75.556835, -71.450869, -77.00968, -71.36337),
      geoid = STATEFP,
      distance = c(55.0671408391101, 97.3321317615295, 121.833438895917, 207.925043587838, 236.989536440657, 259.974710771119, 273.368543029516)
    )
  )
  
  expect_equivalent(
    areas_in_radius("tract", center = c(-109.0452, 36.9991), radius = 24),
    dplyr::tibble(
      STATEFP = c("49", "35", "04", "08"),
      COUNTYFP = c("037", "045", "001", "083"),
      TRACTCE = c("942000", "942801", "942700", "941100"),
      POPULATION = c(3882, 2859, 5628, 1588),
      LATITUDE = c(37.201652, 36.802506, 36.778617, 37.209925),
      LONGITUDE = c(-109.309372, -108.738677, -109.342511, -108.723471),
      geoid = c("49037942000", "35045942801", "04001942700", "08083941100"),
      distance = c(20.1996425651449, 21.7243934916106, 22.4131597365958, 22.9596214504155)
    )
  )
  
  expect_equivalent(
    closest_n_areas(
      geography = "county",
      center = lon_lat_from_area("15007"),
      n = 10,
      units = NULL
    ),
    dplyr::tibble(
      STATEFP = c("15", "15", "15", "15", "15", "02", "02", "06", "06", "06"),
      COUNTYFP = c("007", "003", "005", "009", "001", "016", "013", "045", "023", "097"),
      COUNAME = c("Kauai", "Honolulu", "Kalawao", "Maui", "Hawaii", "Aleutians West", "Aleutians East", "Mendocino", "Humboldt", "Sonoma"),
      STNAME = c("Hawaii", "Hawaii", "Hawaii", "Hawaii", "Hawaii", "Alaska", "Alaska", "California", "California", "California"),
      POPULATION = c(67091L, 953207L, 90L, 154834L, 185079L, 5561L, 3141L, 87841L, 134623L, 483878L),
      LATITUDE = c(22.021022, 21.372464, 21.188495, 20.863747, 19.672837, 54.023571, 54.860151, 39.285329, 40.752982, 38.41408),
      LONGITUDE = c(-159.442112, -157.913673, -156.979972, -156.493816, -155.421895, -168.292885, -162.901536, -123.397227, -124.087189, -122.720522),
      geoid = c("15007", "15003", "15005", "15009", "15001", "02016", "02013", "06045", "06023", "06097"),
      distance = c(0, 173700.997348119, 271092.284047415, 331386.766122282, 492555.274507373, 3630112.85699206, 3657013.88020172, 3912408.72017815, 3912444.49369961, 3939071.16617983)
    )
  )
  
  expect_equivalent(
    closest_population(
      geography = "block group",
      center = lon_lat_from_area(state = "FL"),
      population = 2000,
      units = "barleycorns"
    ),
    dplyr::tibble(
      STATEFP = c("12", "12"),
      COUNTYFP = c("105", "105"),
      TRACTCE = c("015405", "015302"),
      BLKGRPCE = c("1", "2"),
      POPULATION = c(1364L, 1600L),
      LATITUDE = c(27.814011, 27.811222),
      LONGITUDE = c(-81.599316, -81.671874),
      geoid = c("121050154051", "121050153022"),
      distance = c(426753.958691901, 458558.70186313)
    )
  )
  
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