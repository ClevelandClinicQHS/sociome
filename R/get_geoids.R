#' Obtain GEOIDs of places
#'
#' Returns a \code{tibble} of GEOIDs, names, and decennial census population of
#' user-specified locations.
#'
#' This allows users to quickly obtain all GEOIDs in a specified location at a
#' specific level of geography without having to manually look them up somewhere
#' else.
#'
#' This faciliates calls to \code{\link{get_adi}()} that involve somewhat
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
#'
#' @export
get_geoids <- function(geography, state = NULL, county = NULL, geoid = NULL,
                       year = 2010, cache_tables = TRUE, key = NULL, ...) {
  
  geography <- match.arg(geography,
                         c("state", "county", "tract", "block group", "block"))
  
  year      <- floor(year / 10) * 10
  
  if(!(year %in% c(1990, 2000, 2010))) {
    stop("year must be between 1990 and 2019")
  }
  
  variables <- stats::setNames(ifelse(year == 1990, "P0010001", "P001001"),
                               paste0("census_", year, "_pop"))
  
  ref_area  <- validate_location(geoid, state, county, geography, ...)
  
  args <-
    list(
      geography    = geography,
      variables    = variables,
      cache_tables = cache_tables,
      key          = key,
      output       = "wide")
  
  census_data <- call_tidycensus(fn           = tidycensus::get_decennial,
                                 args         = args,
                                 state_county = ref_area$state_county)
  
  if(!is.null(ref_area$geoid)) {
    census_data <- filter_ref_area(data       = census_data,
                                   geoid      = ref_area$geoid,
                                   geo_length = ref_area$geo_length)
  }
  
  return(census_data)
}