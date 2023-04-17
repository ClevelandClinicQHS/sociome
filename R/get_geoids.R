#' Obtain GEOIDs of areas
#'
#' Returns a [`tibble`][tibble::tibble] or [`sf`][sf::sf] of GEOIDs, names, and
#' decennial census population of user-specified locations.
#'
#' This allows users to quickly obtain all GEOIDs in a specified location at a
#' specific level of geography without having to manually look them up somewhere
#' else.
#'
#' This facilitates calls to [get_adi()] that involve somewhat complicated
#' reference areas.
#'
#' @param geography A character string denoting the level of census geography
#'   whose GEOIDs you'd like to obtain. Must be one of `c("state", "county",
#'   "tract", "block group", "block")`.
#'
#'   Note that block-level data cannot be obtained from 1990 and 2000 decennial
#'   census data due to limitations in [tidycensus::get_decennial()]. Whereas
#'   block-level 2010 decennial census data are available, block-level ADI and
#'   ADI-3 cannot be calculated due to the removal of the long-form
#'   questionnaire from the 2010 decennial census.
#' @param year Single integer specifying the year of US Census data to use.
#'   Defaults to 2010. Based on this year, data from the most recent decennial
#'   census will be returned (specifically, `year <- `[`floor`]`(year / 10) *
#'   10` is run).
#' @param state,county,geoid,geometry,cache_tables,key See the descriptions of
#'   the arguments in [get_adi()].
#' @param ... Additional arguments to be passed to
#'   [tidycensus::get_decennial()]. Use at your own risk.
#'
#' @examples
#' \dontrun{
#' # Wrapped in \dontrun{} because it requires a Census API key.
#'
#' # Get all tract GEOIDs for Manhattan
#' tracts <- get_geoids(geography = "tract", state = "New York", county = "New York")
#' tracts
#'
#' # Get all block GEOIDs for the fifth tract on that list
#' get_geoids(geography = "block", geoid = tracts$GEOID[5])
#' }
#' @export
get_geoids <- function(geography,
                       state        = NULL,
                       county       = NULL,
                       geoid        = NULL,
                       year         = 2010,
                       geometry     = FALSE,
                       cache_tables = TRUE,
                       key          = NULL,
                       ...) {
  geography <-
    match.arg(
      geography,
      c("state", "county", "tract", "block group", "block")
    )
  
  # Converts year to the most recent year divisible by 10.
  year <- floor(year / 10) * 10
  if (!any(c(1990, 2000, 2010, 2020) == year)) {
    stop("year must be between 1990 and 2029", call. = FALSE)
  }
  
  # Create the call skeleton to get_decennial() using the validated argument
  # list
  partial_tidycensus_calls <-
    list(
      get_decennial =
        tidycensus_call(
          .fn = "get_decennial",
          geography = geography,
          variables =
            stats::setNames(
              object = if (year == 1990) "P0010001" else "P001001",
              nm = paste0("census_", year, "_pop")
            ),
          table = NULL,
          cache_table = cache_tables,
          year = year,
          sumfile = "sf1",
          geometry = geometry,
          output = "wide",
          # keep_geo_vars = FALSE,
          # summary_var = NULL,
          key = key,
          ...
        )
    )
  
  d <-
    get_tidycensus(
      geography = geography,
      state = state,
      county = county,
      geoid = geoid,
      zcta = NULL,
      year = year,
      dataset = "decennial",
      partial_tidycensus_calls = partial_tidycensus_calls,
      geometry = geometry
    )
  
  d
}
