#' Obtain GEOIDs of places
#'
#' Returns a \code{tibble} of GEOIDs, names, and decennial census population of
#' user-specified locations.
#'
#' This allows users to quickly obtain all GEOIDs in a specified location at a
#' specific level of geography without having to manually look them up somewhere
#' else.
#'
#' This facilitates calls to \code{\link{get_adi}()} that involve somewhat
#' complicated reference areas.
#'
#' @param geography A character string denoting the level of census geography
#'   whose GEOIDs you'd like to obtain. Must be one of \code{c("state",
#'   "county", "tract", "block group", "block")}.
#' @param state A character string specifying states containing the desired
#'   GEOIDs. Defaults to \code{NULL}. Can contain full state names, two-letter
#'   state abbreviations, or two-digit FIPS codes/GEOIDs (must be vector of
#'   strings, so use quotation marks and leading zeros if necessary). Must be
#'   left as \code{NULL} blank if using the \code{geoid} parameter.
#' @param county A vector of character strings specifying the counties
#'   containing the desired GEOIDs. Defaults to \code{NULL}. If specified, the
#'   \code{state} parameter must contain exactly one state. County names and
#'   three-digit FIPS codes are accepted (must contain strings, so use quotation
#'   marks and leading zeros if necessary). Must be blank if using the
#'   \code{geoid} parameter.
#' @param geoid A character vector of GEOIDs (use quotation marks and leading
#'   zeros). Defaults to \code{NULL}. Must be blank if \code{state} and/or
#'   \code{county} is used. Can contain different levels of geography (see
#'   details).
#' @param year Single integer specifying the year of US Census data to use.
#'   Defaults to 2010. Based on this year, data from the most recent decennial
#'   census will be returned.
#' @param geometry,cache_tables,key See the descriptions of the arguments in
#'   \code{\link{get_adi}()}.
#' @param ... Additional arguments to be passed to
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. Not recommended; use
#'   at your own risk.
#'
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
  
  year <- floor(year / 10) * 10
  
  if (!any(c(1990, 2000, 2010) == year)) {
    stop("year must be between 1990 and 2019")
  }
  
  variables <-
    stats::setNames(
      if (year == 1990) "P0010001" else "P001001",
      paste0("census_", year, "_pop")
    )
  
  args <- 
    rlang::dots_list(
      geography = geography,
      variables = variables,
      cache_table = cache_tables,
      year = year,
      sumfile = "sf1",
      geometry = geometry,
      output = "wide",
      key = key,
      ...,
      .homonyms = "first"
    )
  
  partial_call <- rlang::call2(tidycensus::get_decennial, !!!args)
  
  ref_area <-
    validate_location(
      geoid = geoid,
      state = state, 
      county = county, 
      zcta = NULL, 
      geography = geography, 
      dataset = "decennial",
      partial_call = partial_call
    )
  
  census_data <-
    get_tidycensus(partial_call, state_county = ref_area$state_county)
  
  if (!is.null(ref_area$geoid)) {
    census_data <-
      filter_ref_area(
        data       = census_data,
        what       = "GEOID",
        pattern    = ref_area$geoid,
        geo_length = ref_area$geo_length
      )
  }
  
  census_data
}
