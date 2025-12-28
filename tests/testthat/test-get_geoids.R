test_that("get_geoids()_works", {
  expect_error(
    get_geoids(geography = "state", year = 1776),
    "(?i)year must be between"
  )

  # State 2000
  expect_snapshot(
    get_geoids(
      geography = "state",
      state = c("OH", "IN"),
      year = 2005,
      geometry = TRUE
    )
  )
  # State 2010
  expect_snapshot(
    get_geoids(
      geography = "state",
      state = c("OH", "IN"),
      year = 2012,
      geometry = TRUE
    )
  )
  # State 2020
  expect_snapshot(
    get_geoids(
      geography = "state",
      state = c("OH", "IN"),
      year = 2025,
      geometry = TRUE
    )
  )

  # County 2000
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2005,
      geometry = TRUE
    )
  )
  # County 2010
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2012,
      geometry = TRUE
    )
  )
  # County 2020
  expect_snapshot(
    get_geoids(
      geography = "county",
      state = c("DE", "DC"),
      year = 2025,
      geometry = TRUE
    )
  )


  # Tract 2000
  expect_snapshot(
    get_geoids(
      geography = "tract",
      state = c("DE", "DC"),
      year = 2005,
      geometry = TRUE
    )
  )
  # Tract 2010
  expect_snapshot(
    get_geoids(
      geography = "tract",
      state = c("DE", "DC"),
      year = 2012,
      geometry = TRUE
    )
  )
  # Tract 2020
  expect_snapshot(
    get_geoids(
      geography = "tract",
      state = c("DE", "DC"),
      year = 2025,
      geometry = TRUE
    )
  )


  # Block group 2000
  expect_snapshot(
    get_geoids(
      geography = "block group",
      state = c("DE", "DC"),
      year = 2005,
      geometry = TRUE
    )
  )
  # Block group 2010
  expect_snapshot(
    get_geoids(
      geography = "block group",
      state = c("DE", "DC"),
      year = 2012,
      geometry = TRUE
    )
  )
  # Block group 2020
  expect_snapshot(
    get_geoids(
      geography = "block group",
      state = c("DE", "DC"),
      year = 2025,
      geometry = TRUE
    )
  )
})
