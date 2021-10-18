
#' Make a tibble of census areas closest to a user-specified center
#'
#' Returns a [`tibble`][tibble::tibble] containing the census areas whose
#' centers of population are closest to some user-specified center. To specify
#' the center, the user can manually enter longitude/latitude coordinates or use
#' the helper function [lon_lat_from_area()] to automatically grab the
#' longitude/latitude coordinates of the center of population of an area. The
#' cutoff point for how many areas will be return depends on the function used.
#'
#' `areas_in_radius()` returns all areas whose centers of population are within
#' the user-specified `radius` around `center`.
#'
#' `closest_n_areas()` returns the top `n` areas whose centers of population are
#' closest areas to `center`.
#'
#' Conceptually, `closest_population()` sequentially gathers the next closest
#' area to `center` until the total population of the areas meets or exceeds
#' `population`.
#'
#' Distances are determined with [geosphere::distm()].
#'
#' Requires the packages `USpopcenters` and `geosphere` to be installed.
#' Requires the `units` to be installed unless `units = NULL`.
#'
#' Centers of population are based on the 2010 decennial census. See
#' <https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.2010.html>
#' Only states, counties, tracts, and block groups are currently supported.
#'
#' @param geography The type of census areas that the resulting table will
#'   contain. One of `c("state", "county", "tract", "block group")`.
#' @param center The longitude/latitude coordinates of the center of the circle.
#'   A double vector of length 2 whose elements are finite numbers. Passed to
#'   the `y` argument of [geosphere::distm()].
#'
#'   The first element is the longitude coordinate (positive for west, negative
#'   for east). The second element is the latitude coordinate (positive for
#'   north, negative for south).
#'
#'   The convenience function [lon_lat_from_area()] can be used to obtain the
#'   longitude/latitude coordinates of the center of population of a
#'   user-specified census area.
#'
#'   Defaults to the center of population of the District of Columbia according
#'   to the 2010 decennial census.
#' @param radius A single, non-negative number specifying the radius of the
#'   circle. Defaults to 5.
#' @param units A single string specifying the units of the resulting `distance`
#'   column. If `NULL`, the `units` package does not need to be installed, and
#'   units will be meters. Otherwise, this will be passed to the `value`
#'   argument of [`units::set_units`]`(mode = "standard")`.
#'
#'   For `areas_in_radius()`, this also used for the units of `radius`.
#' @param n A single positive integer specifying how many of the areas closest
#'   to `center` should be gathered. Defaults to 50.
#' @param population A single positive integer specifying the target total
#'   population of the areas returned. See **Details**.
#' @param measure_from Currently can only be `"center of population"`, the
#'   default.
#' @param year Must be either 2000 or the default of 2010.
#' @param distance_fun Passed to the `fun` argument of [geosphere::distm()].
#'   Defaults to [`geosphere::distVincentyEllipsoid`], which results in the most
#'   accurate measurement but is also the slowest.
#' @param batch_size The number of distances calculated in each iterative call
#'   to [geosphere::distm()]. When the request is satisfied, these functions
#'   stop calculating distances in order to prevent potentially hundreds of
#'   thousands of unnecessary calculations. Defaults to 50.
#'
#' @examples
#' # All states whose centers of population are within 300 kilometers of the
#' # center of population of New York County, New York (i.e, Manhattan):
#' areas_in_radius(
#'   geography = "state",
#'   center = lon_lat_from_area(state = "NY", county = "New York"),
#'   radius = 300,
#'   units = "km"
#' )
#'
#' # The four census tracts whose centers of population are closest to the
#' # Four Corners (distance column is in meters due to setting units = NULL):
#' closest_n_areas("tract", center = c(-109.0452, 36.9991), n = 4, units = NULL)
#'
#' # The counties closest to center of population of Kauai County, Hawaii whose
#' # total population reaches 3 million people:
#' closest_population(  
#'   geography = "county",
#'   center = lon_lat_from_area("15007"),
#'   population = 3e6,
#'   units = "barleycorns"
#' )
#' @return A [`tibble`][tibble::tibble] with each of the columns found in the
#'   corresponding `USpopcenters` table, with two columns appended:
#'
#'   `geoid` - all FIPS code columns combined with [paste0()].
#'
#'   `distance` - the number of `units` the area's `LONGITUDE`/`LATITUDE` center
#'   of population is away from the coordinates given in `center`.
#' @seealso [lon_lat_from_area()]
#' @name get_areas_near_coordinates
#' @export
areas_in_radius <- function(geography = c("state", "county", "tract",
                                          "block group"),
                            center = lon_lat_from_area(state = "DC"),
                            radius = 5,
                            units = "miles",
                            measure_from = "center of population",
                            year = 2010,
                            distance_fun = geosphere::distVincentyEllipsoid,
                            batch_size = 50L) {
  check_for_packages(c("USpopcenters", "geosphere"))
  
  if (!is.numeric(radius) || length(radius) != 1L || is.na(radius) ||
      (radius <- as.numeric(radius)) < 0) {
    stop("radius must be a single non-negative number", call. = FALSE)
  }
  
  if (!is.null(units)) {
    check_for_packages("units")
    radius <- radius %>% 
      units::set_units(units, mode = "standard") %>% 
      units::set_units("meters", mode = "standard") %>% 
      as.numeric()
  }
  
  if (!identical(as.character(measure_from), "center of population")) {
    stop('meaure_from currently must equal "center of population"',
         call. = FALSE)
  }
  
  geography <- match.arg(geography)
  year <- validate_year(year, c(2010, 2000))
  
  batch_size <- validate_single_positive_integer(batch_size, "batch_size")
  
  all_centers <- centers_tbl_from_geography(geography, year)
  
  center <- validate_lon_lat(center)
  
  filter_centers(
    all_centers = all_centers,
    lon_lat = center, 
    distance_fun = distance_fun,
    target = radius,
    target_type = "radius", 
    units = units,
    batch_size = batch_size
  )
}





#' @rdname get_areas_near_coordinates
#' @export
closest_n_areas <- function(geography = c("state", "county", "tract",
                                          "block group"),
                            center = lon_lat_from_area(state = "DC"),
                            n = 50,
                            measure_from = "center of population",
                            year = 2010,
                            distance_fun = geosphere::distVincentyEllipsoid,
                            units = NULL,
                            batch_size = 50L) {
  check_for_packages(
    c("USpopcenters", "geosphere", if (!is.null(units)) "units")
  )
  
  if (!identical(as.character(measure_from), "center of population")) {
    stop('meaure_from currently must equal "center of population"',
         call. = FALSE)
  }
  
  geography <- match.arg(geography)
  year <- validate_year(year, c(2010, 2000))
  n <- validate_single_positive_integer(n, "n")
  batch_size <- validate_single_positive_integer(batch_size, "batch_size")
  
  all_centers <- centers_tbl_from_geography(geography, year)
  
  center <- validate_lon_lat(center)
  
  filter_centers(
    all_centers = all_centers,
    lon_lat = center, 
    distance_fun = distance_fun,
    target = n,
    target_type = "n", 
    units = units,
    batch_size = batch_size
  )
}





#' @rdname get_areas_near_coordinates
#' @export
closest_population <- function(geography = c("state", "county", "tract",
                                             "block group"),
                               center = lon_lat_from_area(state = "DC"),
                               population = 1e6,
                               measure_from = "center of population",
                               year = 2010,
                               distance_fun = geosphere::distVincentyEllipsoid,
                               units = NULL,
                               batch_size = 50L) {
  check_for_packages(
    c("USpopcenters", "geosphere", if (!is.null(units)) "units")
  )
  
  if (!identical(as.character(measure_from), "center of population")) {
    stop('meaure_from currently must equal "center of population"',
         call. = FALSE)
  }
  
  geography <- match.arg(geography)
  year <- validate_year(year, c(2010, 2000))
  population <- validate_single_positive_integer(population, "population")
  batch_size <- validate_single_positive_integer(batch_size, "batch_size")
  
  all_centers <- centers_tbl_from_geography(geography, year)
  
  center <- validate_lon_lat(center)
  
  filter_centers(
    all_centers = all_centers,
    lon_lat = center, 
    distance_fun = distance_fun,
    target = population,
    target_type = "population", 
    units = units,
    batch_size = batch_size
  )
}





#' @importFrom rlang .data
filter_centers <- function(all_centers,
                           lon_lat, 
                           distance_fun,
                           target,
                           target_type = c("radius", "n", "population"), 
                           units = NULL,
                           batch_size = 50L) {
  all_centers <-
    dplyr::arrange(
      all_centers,
      abs(.data$LONGITUDE - !!lon_lat[1L]) + abs(.data$LATITUDE - !!lon_lat[2L])
    )
  
  all_centers$distance <- Inf
  
  break_condition <-
    switch(
      target_type,
      
      # All distances in the batch are greater than the radius
      radius =
        function() all(all_centers$distance[rows] > target, na.rm = TRUE),
      
      # The first row in the batch is at least n + 1 (i.e., n was reached in
      # the previous batch)
      n = function() target + 1L <= rows[1L],
      
      # The total population of all previous batches meets or exceeds the
      # target population
      population =
        function() target <=
        sum(all_centers$POPULATION[1L:(rows[1L] - 1L)], na.rm = TRUE)
    )
  
  for (rows in row_number_batches(all_centers)) {
    all_centers$distance[rows] <- 
      as.numeric(
        geosphere::distm(
          x = as.matrix(all_centers[rows, c("LONGITUDE", "LATITUDE")]),
          y = lon_lat,
          fun = distance_fun
        )
      )
    
    # Stop calculating distances if the break condition is met. 
    if (break_condition()) break
  }
  
  all_centers <-
    dplyr::arrange(all_centers, .data$distance, .data$POPULATION, .data$geoid)
  
  out <-
    switch(
      target_type,
      radius = all_centers[all_centers$distance <= target, ],
      n = all_centers[seq_len(target), ],
      population = 
        all_centers[
          dplyr::lag(cumsum(all_centers$POPULATION) < target, default = TRUE),
        ]
    )
  
  if (!is.null(units)) {
    out$distance <- out$distance %>% 
      units::set_units("meters", mode = "standard") %>% 
      units::set_units(units, mode = "standard") %>% 
      as.numeric()
  }
  
  out
} 






#' Grab the longitude/latitude of the center of population of a census area
#'
#' The user specifies a census area, and the function returns the
#' longitude/latitude coordinates of the area's center of population according
#' to the 2010 census.
#'
#' Centers of population are based on the 2010 decennial census. See
#' <https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.2010.html>
#' Only states, counties, tracts, and block groups are currently supported.
#'
#' Requires the data package `USpopcenters` to be installed.
#'
#' @param geoid A single string specifying the geoid of a census area. Must be
#'   2, 5, 11, or 12 digits. Must be `NULL` if `state` is not `NULL`.
#' @param state A single string containing the FIPS code, two-letter
#'   abbreviation, or full state name of a US state or the District of Columbia
#'   or Puerto Rico. Not case sensitive. Must be `NULL` if `geoid` is not
#'   `NULL`.
#' @param county A single string specifying the name of a county in `state` or
#'   the three- or five-digit GEOID of a county in `state`. Not case sensitive.
#'   If entering a county name, it must match the beginning of only one of the
#'   county names in `state`. If entering a five-digit GEOID, it will throw an
#'   error if its first two digits do not match the GEOID of `state`. Must be
#'   `NULL` if state is `NULL`.
#' @param year Either 2000 or the default of 2010.
#'
#' @examples
#' # The center of population of Alaska
#' lon_lat_from_area(state = "alAskA")
#'
#' # The center of population of Cook County, Illinois.
#' lon_lat_from_area(state = "IL", county = "Cook")
#'
#' # The center of population of some tract in Manhattan
#' lon_lat_from_area(geoid = "36061021600")
#'
#' @return A [double] vector of length 2. The first element is LONGITUDE
#'   (positive for east, negative for west). The second element is LATITUDE
#'   (positive for north, negative for south).
#' @seealso [areas_in_radius()]
#' @export
lon_lat_from_area <- function(geoid = NULL, 
                              state = NULL,
                              county = NULL, 
                              year = 2010) {
  check_for_packages("USpopcenters")
  year <- validate_year(year, c(2010, 2000))
  switch(
    determine_input_arg(geoid = geoid, state = state, county = county),
    geoid = lon_lat_from_geoid(geoid, year = year),
    state = lon_lat_from_state(state, year = year),
    county = lon_lat_from_county(state, county, year = year),
    # zcta = stop("ZCTA not yet supported", call. = FALSE),
    stop("Must enter geoid or state/county", call. = FALSE)
  )
}



lon_lat_from_geoid <- function(geoid, year) {
  geoid <- validate_single_geoid(geoid)
  tbl <- centers_tbl_from_geoid(geoid, year)
  
  i <- which(tbl$geoid == geoid)
  
  if (length(i) != 1L) {
    if (length(i)) {
      stop('geoid = "', geoid, '" somehow matched multiple areas.',
           "\nPlease post an issue to GitHub.", call. = FALSE)
    }
    stop('geoid = "', geoid, '" did not match any areas for year ', year,
         call. = FALSE)
  }
  
  as.double(tbl[i, c("LONGITUDE", "LATITUDE")])
}


lon_lat_from_state <- function(state, year) {
  state <- validate_state(state)
  lon_lat_from_geoid(state, year)
}



lon_lat_from_county <- function(state, county, year) {
  state <- validate_state(state)
  county <- validate_county(state, county)
  lon_lat_from_geoid(county, year)
}






row_number_batches <- function(all_centers, batch_size = 50L) {
  all_centers %>% 
    nrow() %>% 
    seq_len() %>% 
    split(rep(., each = batch_size, length.out = length(.)))
}




centers_tbl_from_geography <- function(geography, year) {
  tbl <-
    switch(
      paste0(geography, year),
      state2010 = USpopcenters::state2010,
      county2010 = USpopcenters::county2010,
      tract2010 = USpopcenters::tract2010,
      "block group2010" = USpopcenters::block_group2010,
      state2000 = USpopcenters::state2000,
      county2000 = USpopcenters::county2000,
      tract2000 = USpopcenters::tract2000,
      "block group2000" = USpopcenters::block_group2000,
      stop("Currently geography must be state, county, tract, or block group",
           call. = FALSE)
    )
  USpopcenters_to_geoid_tbl(tbl)
}


centers_tbl_from_geoid <- function(geoid, year) {
  tbl <-
    if (year == 2010) {
      switch(
        match(nchar(geoid), c(2L, 5L, 11L, 12L), nomatch = 5L),
        USpopcenters::state2010,
        USpopcenters::county2010,
        USpopcenters::tract2010,
        USpopcenters::block_group2010,
        stop("geoid must have 2, 5, 11, or 12 digits", call. = FALSE)
      )
    } else if (year == 2000) {
      switch(
        match(nchar(geoid), c(2L, 5L, 11L, 12L), nomatch = 5L),
        USpopcenters::state2000,
        USpopcenters::county2000,
        USpopcenters::tract2000,
        USpopcenters::block_group2000,
        stop("geoid must have 2, 5, 11, or 12 digits", call. = FALSE)
      )
    } else {
      stop("year must be 2010 or 2000", call. = FALSE)
    }
  
  USpopcenters_to_geoid_tbl(tbl)
}


USpopcenters_to_geoid_tbl <- function(tbl) {
  tbl$geoid <-
    do.call(
      paste0,
      tbl[
        intersect(c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE"), names(tbl))
      ]
    )
  
  tbl
}
