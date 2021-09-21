
#' Make a tibble of census areas within a user-specified circle
#'
#' Returns a table containing all census areas whose centers of population are
#' within a radius of some center. To specify the center, the user can manually
#' enter longitude/latitude coordinates or use the helper function
#' [lon_lat_from_area()] to automatically grab the longitude/latitude
#' coordinates of the center of population of an area.
#'
#' Centers of population are based on the 2010 decennial census. See
#' <https://www.census.gov/geographies/reference-files/time-series/geo/centers-population.2010.html>
#' Only states, counties, tracts, and block groups are currently supported.
#'
#' Distances are determined with [geosphere::distm()].
#'
#' Requires the packages `USpopcenters`, `geosphere`, and `units` to be
#' installed.
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
#' @param radius A single, non-missing number specifying the radius of the
#'   circle. Processed with [as.double()]. Defaults to 5.
#' @param units A single string specifying the units of `radius`. Passed to the
#'   `value` argument of [`units::set_units`]`(mode = "standard")`. Defaults to
#'   `"miles"`.
#' @param measure_from Currently can only be `"center of population"`, the
#'   default.
#' @param year Must be either 2000 or the default of 2010.
#' @param distance_fun Passed to the `fun` argument of [geosphere::distm()].
#'   Defaults to [`geosphere::distVincentyEllipsoid`], which results in the most
#'   accurate measurement but is also the slowest.
#'
#' @examples
#' # All states within 300 kilometers of the center of population of Manhattan
#' areas_in_radius(
#'   geography = "state",
#'   center = lon_lat_from_area(state = "NY", county = "New York"),
#'   radius = 300,
#'   units = "km"
#' )
#'
#' # All census tracts within 24 miles of the four corners
#' areas_in_radius("tract", center = c(-109.0452, 36.9991), radius = 24)
#' @return A [`tibble`][tibble::tibble] with columns `geoid`, `longitude`,
#'   `latitude`, and `distance`. The `longitude` and `latitude` are the center
#'   of population of the area, and `distance` is the number of `units` the
#'   area's center of population is away from the coordinates given in `center`.
#' @seealso [lon_lat_from_area()]
#' @export
areas_in_radius <- function(geography,
                            center = lon_lat_from_area(state = "DC"),
                            radius = 5,
                            units = "miles",
                            measure_from = "center of population",
                            year = 2010,
                            distance_fun = geosphere::distVincentyEllipsoid) {
  check_for_packages(c("USpopcenters", "geosphere", "units"))
  
  if (!identical(as.character(measure_from), "center of population")) {
    stop('meaure_from currently must equal "center of population"',
         call. = FALSE)
  }
  
  geography <-
    match.arg(geography, c("state", "county", "tract", "block group"))
  year <- validate_year(year, c(2010, 2000))
  
  all_centers <- centers_tbl_from_geography(geography, year)
  
  center <- validate_lon_lat(center)
  
  filter_centers(all_centers, center, radius, units, distance_fun)
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
#' @return A [double] vector of length 2. The first element is longitude
#'   (positive for east, negative for west). The second element is latitude
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
  
  as.double(tbl[i, c("longitude", "latitude")])
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





#' @importFrom rlang .data
filter_centers <- function(all_centers,
                           lon_lat, 
                           radius, 
                           units, 
                           distance_fun) {
  all_centers <-
    dplyr::arrange(
      all_centers,
      sqrt(
        (.data$longitude - !!lon_lat[1L]) ^ 2L +
          (.data$latitude - !!lon_lat[2L]) ^ 2L
      )
    )
  
  all_centers$distance <- Inf
  
  row_number_batches <-
    all_centers %>% 
    nrow() %>% 
    seq_len() %>% 
    split(rep(., each = 50L, length.out = length(.)))
  
  for (rows in row_number_batches) {
    all_centers$distance[rows] <- 
      geosphere::distm(
        x = as.matrix(all_centers[rows, c("longitude", "latitude")]),
        y = lon_lat,
        fun = distance_fun
      ) %>% 
      as.numeric() %>% 
      units::set_units("meters", mode = "standard") %>% 
      units::set_units(units, mode = "standard") %>%
      as.numeric()
    
    if (all(all_centers$distance[rows] > radius)) break
  }
  
  all_centers[all_centers$distance <= radius, ]
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


#' @importFrom rlang .data
USpopcenters_to_geoid_tbl <- function(tbl)
  dplyr::transmute(
    tbl,
    geoid =
      do.call(
        paste0, 
        dplyr::across(
          dplyr::any_of(c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE"))
        )
      ),
    longitude = .data$LONGITUDE,
    latitude = .data$LATITUDE
  )