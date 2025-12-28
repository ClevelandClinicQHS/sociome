test_that("area population functions work", {

  if (requireNamespace("USpopcenters", quietly = TRUE) &&
      requireNamespace("geosphere", quietly = TRUE)) {

    expect_error(lon_lat_from_area(), "Must enter geoid or state/county")
    expect_snapshot(lon_lat_from_area("39035"))
    expect_snapshot(lon_lat_from_area(state = "oh", county = "cuyahoga"))

    expect_error(lon_lat_from_geoid("390356942069", year = 2000), "did not match any areas for year")

    expect_error(areas_in_radius(radius = NULL), "radius must be a single non-negative number")
    expect_error(areas_in_radius(radius = numeric()), "radius must be a single non-negative number")
    expect_error(areas_in_radius(radius = NA_real_), "radius must be a single non-negative number")
    expect_error(areas_in_radius(radius = -1), "radius must be a single non-negative number")

    expect_snapshot(
      areas_in_radius(
        geography = "state",
        center =
          lon_lat_from_area(state = "NY", county = "New York", year = 2010),
        year = 2010,
        radius = 300,
        units = "km"
      )
    )

    expect_snapshot(
      areas_in_radius(
        "tract",
        center = c(-109.0452, 36.9991),
        year = 2010,
        radius = 24
      )
    )

    expect_snapshot(
      closest_n_areas(
        geography = "county",
        center = lon_lat_from_area("15007", year = 2010),
        year = 2010,
        n = 10,
        units = NULL
      )
    )

    expect_snapshot(
      closest_population(
        geography = "block group",
        center = lon_lat_from_area(state = "FL", year = 2010),
        year = 2010,
        population = 2000,
        units = "barleycorns"
      )
    )
  }
})
