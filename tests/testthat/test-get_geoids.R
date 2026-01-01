test_that("get_geoids()_works", {

  skip_if(Sys.getenv("CENSUS_API_KEY") == "")

  expect_error(
    get_geoids(geography = "state", year = 1776, dataset = "decennial"),
    "decennial census data are only available for 2000, 2010,( and)? 2020"
  )

  # State 2000
  expect_snapshot(
    get_geoids(
      geography = "state",
      state = c("OH", "IN"),
      year = 2000,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # State 2010
  expect_snapshot(
    get_geoids(
      geography = "state",
      state = c("OH", "IN"),
      year = 2010,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # State 2020
  expect_snapshot(
    get_geoids(
      geography = "state",
      state = c("OH", "IN"),
      year = 2020,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )

  # County 2000
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2000,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # County 2010
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2010,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # County 2020
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2020,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )


  # Tract 2000
  expect_snapshot(
    get_geoids(
      geography = "tract",
      state = c("DE", "DC"),
      year = 2000,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # Tract 2010
  expect_snapshot(
    get_geoids(
      geography = "tract",
      state = c("DE", "DC"),
      year = 2010,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # Tract 2020
  expect_snapshot(
    get_geoids(
      geography = "tract",
      state = c("DE", "DC"),
      year = 2020,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )


  # Block group 2000
  expect_snapshot(
    get_geoids(
      geography = "block group",
      state = c("DE", "DC"),
      year = 2000,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # Block group 2010
  expect_snapshot(
    get_geoids(
      geography = "block group",
      state = c("DE", "DC"),
      year = 2010,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # Block group 2020
  expect_snapshot(
    get_geoids(
      geography = "block group",
      state = c("DE", "DC"),
      year = 2020,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )

  # Block 2000
  expect_snapshot(
    get_geoids(
      geography = "block",
      state = "HI",
      county = "Kalawao",
      year = 2000,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # Block 2010
  expect_snapshot(
    get_geoids(
      geography = "block",
      state = "HI",
      county = "Kalawao",
      year = 2010,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # Block 2020
  expect_snapshot(
    get_geoids(
      geography = "block",
      state = "HI",
      county = "Kalawao",
      year = 2020,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )



  # ZCTA 2000
  expect_warning(
    zcta2000 <-
      get_geoids(
        geography = "zcta",
        # state = "WY",
        year = 2000,
        dataset = "decennial",
        geometry = TRUE,
        progress_bar = FALSE
      ),
    "CB ZCTAs for 2000 include separate polygons for discontiguous parts"
  )
  expect_snapshot(zcta2000)

  # ZCTA 2010
  expect_snapshot(
    get_geoids(
      geography = "zcta",
      state = "WY",
      year = 2010,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
  # ZCTA 2020
  expect_snapshot(
    get_geoids(
      geography = "zcta",
      state = "WY",
      year = 2020,
      dataset = "decennial",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )

  # ACS1 2005
  expect_snapshot(
    get_geoids(
      geography = "state",
      year = 2005,
      dataset = "acs1",
      # progress_bar = FALSE,
      geometry = FALSE
    )
  )

  # ACS3 2012
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2012,
      dataset = "acs3",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )

  # ACS5 2022
  expect_snapshot(
    get_geoids(
      geography = "tract",
      geoid = c("10", "39035"),
      year = 2012,
      dataset = "acs5",
      geometry = TRUE,
      progress_bar = FALSE
    )
  )
})
