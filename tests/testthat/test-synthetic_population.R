test_that("synthetic_population() works", {
  expect_snapshot(
    synthetic_population(geography = "state", state = "UT", year = 2019)
  )

  # Same, but make it so that survival past age 85 is highly unlikely
  # (via rate = 10), and so that 87 is the maximum possible age
  expect_snapshot(
    synthetic_population(
      geography = "state",
      state = "UT",
      year = 2019,
      max_age = 87,
      rate = 10
    )
  )

  # Synthetic population of the Delmarva Peninsula at the census tract level,
  # using 2000 Decennial Census data
  expect_snapshot(
    synthetic_population(
      geography = "tract",
      geoid =
        # This two-digit GEOID is the state of Delaware.
        c("10",

          # These five-digit GEOIDs are specific counties in Virginia and Maryland
          "51001", "51131", "24015", "24029", "24035", "24011", "24041", "24019",
          "24045", "24039", "24047"),
      year = 2000,
      dataset = "decennial"
    )
  )
})
