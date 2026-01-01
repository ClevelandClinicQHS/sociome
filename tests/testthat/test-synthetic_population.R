test_that("synthetic_population() works", {
  set.seed(20251229)

  expect_snapshot(
    synthetic_population(
      geography = "state",
      state = "UT",
      year = 2019,
      max_age = 87,
      rate = 10
    )
  )

  # Synthetic population of blocks in mixed GEOIDs
  expect_snapshot(
    synthetic_population(
      geography = "block",
      geoid = c("51001", "40117957200", "490572105041"),
      year = 2000,
      dataset = "decennial"
    )
  )

  # Synthetic population of some random ZCTAs
  expect_snapshot(
    synthetic_population(
      geography = "zcta",
      state = c("CA", "OH", "AL"),
      zcta = c("90210", "44133", "44147", "44136", "01001", "01010"),
      year = 2013,
      dataset = "acs5"
    )
  )

  # Synthetic population
  expect_snapshot(
    synthetic_population(
      geography = "county",
      state = "NY",
      county = c("New York", "Queen", "King", "Bronx", "Richmond"),
      year = 2005,
      dataset = "acs1"
    )
  )
})
