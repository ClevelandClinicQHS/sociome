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
#'
#'   Note that block-level data cannot be obtained from 1990 and 2000 decennial
#'   census data due to limitations in
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. Whereas block-level
#'   2010 decennial census data are available, block-level ADIs cannot be
#'   calculated due to the removal of the long-form questionnaire from the 2010
#'   decennial census.
#' @param year Single integer specifying the year of US Census data to use.
#'   Defaults to 2010. Based on this year, data from the most recent decennial
#'   census will be returned (specifically, \code{year <- floor(year / 10) * 10}
#'   is run).
#' @param state,county,geoid,geometry,cache_tables,key See the descriptions of
#'   the arguments in \code{\link{get_adi}()}.
#' @param ... Additional arguments to be passed to
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. Not recommended; use
#'   at your own risk.
#'
#' @examples
#' \donttest{
#' # Wrapped in \donttest{} because it requires a Census API key.
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
  
  year <- floor(year / 10) * 10
  
  if (!any(c(1990, 2000, 2010) == year)) {
    stop("year must be between 1990 and 2019")
  }
  
  variables <-
    stats::setNames(
      object = if (year == 1990) "P0010001" else "P001001",
      nm = paste0("census_", year, "_pop")
    )
  
  args <-
    list(
      geography = geography,
      variables = variables,
      cache_table = cache_tables,
      year = year,
      sumfile = "sf1",
      geometry = geometry,
      output = "wide",
      key = key,
      endyear = rlang::zap(),
      survey = rlang::zap(),
      ...
    )
  if (!rlang::is_named(args)) {
    stop("\nAdditional arguments passed to ... must all be named")
  }
  
  tidycensus_call <-
    rlang::call_modify(
      .call = quote(tidycensus::get_decennial()),
      !!!args,
      .homonyms = "first"
    ) %>% 
    list()
  
  ref_area <-
    validate_location(
      geoid = geoid,
      state = state, 
      county = county, 
      zcta = NULL, 
      geography = geography, 
      year = year,
      dataset = "decennial",
      tidycensus_calls = tidycensus_call
    )
  
  census_data <- 
    ref_area$state_county %>% 
    dplyr::mutate(.call = tidycensus_call) %>% 
    purrr::pmap(rlang::call_modify) %>% 
    lapply(eval) %>% 
    do.call(what = rbind) %>% 
    dplyr::select("GEOID", "NAME", dplyr::starts_with("census_"))
  
  if (!is.null(ref_area$geoid)) {
    census_data <- census_data %>% 
      filter_ref_area(
        what       = "GEOID",
        pattern    = ref_area$geoid,
        geo_length = ref_area$geo_length
      )
  }
  
  census_data
}
