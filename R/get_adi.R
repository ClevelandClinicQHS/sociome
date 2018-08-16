#' Calculates ADIs of user-specified areas.
#'
#' Returns a tibble of the area deprivation indices (ADIs) of user-specified
#' locations in the United States, utilizing American Community Survey data or
#' decennial US census data.
#'
#' @param geography A character string denoting the level of census geography
#'   whose ADIs you'd like to obtain. Must be one of c("state", "county",
#'   "tract", or "block group"). Defaults to NULL. See details for default
#'   behaviors when this and other parameters are left blank.
#' @param state A vector of character strings specifying the state(s) whose ADI
#'   data you're requesting. Defaults to NULL. Can contain full state names,
#'   two-letter state abbreviations, or FIPS codes/GEOIDs (must be a string, so
#'   use quotation marks and leading zeros). Must contain exactly one state if
#'   the parameter \code{counties} is also used. Must be blank if \code{geoid}
#'   is used.
#' @param county A vector of character strings specifying the counties whose ADI
#'   data you're requesting. Defaults to NULL. County names and three-digit FIPS
#'   codes are accepted (must contain strings, so use quotation marks and
#'   leading zeros). All elements must be in the same state; therefore, if using
#'   \code{county}, \code{state} must contain exactly one value. Must be blank
#'   if \code{geoid} is used.
#' @param geoid A character vector of GEOIDs (use quotation marks and leading
#'   zeros). Defaults to NULL. Must be blank if \code{state} and/or
#'   \code{county} is used. Can contain different levels of geography (see
#'   details).
#' @param year Single integer specifying the year of the ACS survey or decennial
#'   census data to use. Default for ACS surveys is 2016 and default for
#'   decennial census is 2010.
#' @param survey The data set used to calculate ADIs. Must be one of c("acs5",
#'   "acs3", "acs1", or "census"), with the latter denoting the decennial US
#'   census and the others denoting the 5-, 3-, and 1-year ACS estimates.
#'   Defaults to "acs5." Important: data not always available depending on the
#'   level of geography. See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html}.
#' @param geometry Logical value indicating whether or not shapefile data should
#'   be included in the tibble. Defaults to TRUE.
#' @param key Your Census API key as a character string. Obtain one at
#'   http://api.census.gov/data/key_signup.html. Defaults to null. Not necessary
#'   if you have already loaded your key with \code{\link{census_api_key}}.
#'
#' @return A tibble of three columns: GEOID of location, Name of location, ADI
#'   of location
#'
#' @details The algorithm that produced the original ADIs employs factor
#'   analysis. As a result, the ADI is a relative measure; the ADI of a
#'   particular location is dynamic, varying depending on which other locations
#'   were supplied to the algorithm. In other words, ADI will vary depending on
#'   the reference area. For example, the ADI of Orange County, California is
#'   \emph{x} when calculated alongside all other counties in California, but it
#'   is \emph{y} when calculated alongside all counties in the US. The get_adi()
#'   function enables the user to define a reference area by feeding a vector of
#'   GEOIDs to its \code{geoid} parameter (or alternatively for convenience, a
#'   vector of state and county names/abbreviations to \code{state} and
#'   \code{county}). The function then gathers data from those specified
#'   locations and performs calculations using their data alone.
#'
#'   If \code{geography} is left blank, the function will choose the most
#'   specific level of geography specified by the parameter(s) \code{state},
#'   \code{county}, and/or \code{geoid}. If \code{geography} is specified but
#'   \code{state}, \code{county}, and \code{geoid} are all left blank, the
#'   function will use the entire US as the reference area (see README.md for a
#'   discussion of reference area). If all these parameters are left blank, the
#'   function will report the ADIs of all census tracts in the United States.
#'
#'   Elements of \code{geoid} can represent different levels of geography, but
#'   they all must be either 2 digits (for states), 5 digits (for counties), 11
#'   digits (for tracts) or 12 digits (for block groups). Must contain character
#'   strings, so use quotation marks as well as leading zero where applicable.
#'
#' @examples
#' get_adi(geography = "tract", state = "OH", county = "Cuyahoga")
#' 
#' get_adi(geography = "county", state = "CT", year = 2015, survey = "acs1", geometry = FALSE)
#'   
#' delmarva <- get_adi(geoids = c("10", "51001", "51131", "24015", "24029", "24035", "24011", "24041", "24019", "24045", "24039", "24047"))
#' 
#' # Demonstration of geom_sf integration:
#' \dontrun{
#' delmarva %>% ggplot() + geom_sf(aes(fill = ADI))
#' }
#'
#' @return A tibble with four columns: \code{GEOID}, \code{NAME}, \code{GEOID},
#'   and \code{geometry} (which is left out if \code{geometry = FALSE} is
#'   specified).
#'
#' @export

get_adi <- function(geography = NULL,
                    state     = NULL,
                    county    = NULL,
                    geoids    = NULL,
                    year      = NULL,
                    survey    = "acs5",
                    geometry  = TRUE,
                    key       = NULL,
                    ...
                    ) {
  
  if(!is.null(county)) {
    
    # Throws error if user supples counties but doesn't supply exactly one state
    if(length(state) != 1) {
      stop("If supplying counties, exactly one state must be provided")
    }
    
    # Throws error if user supplies county, state, and geoids
    if(!is.null(geoids)) {
      stop("If supplying geoids, state and county must be NULL")
    }
    
    # Validates user-supplied state and coerces it into its two-digit GEOID
    state <- tidycensus:::validate_state(state)
    
    # Validates user-supplied county values and populates geoids with the
    # counties' five-digit GEOIDs
    geoids <-
      unique(sapply(county,
                    function(x) {
                      paste0(state, 
                             tidycensus:::validate_county(state = state,
                                                          county = x))
                    }))
  }
  
  
  else if(!is.null(state)) {

    # Throws error if user supplies both states and geoids
    if(!is.null(geoids)) {
      stop("If supplying geoids, state and county must be NULL")
    }
    
    # Populates geoids with states' two-digit geoids.
    else {
      geoids <- unique(sapply(state, tidycensus:::validate_state))
    }
    
  }
  
  ref_area <- get_reference_area(geoids, geography)
  
  tidycensus_args <-
    list(
      geography = ref_area$geography,
      year = year, survey = survey, key = key, geometry = geometry,
      cache_table = TRUE, output = "wide",
      variables =
        c("B01003_001","B19013_001","B19001_002","B19001_011","B19001_012",
          "B19001_013","B19001_014","B19001_015","B19001_016","B19001_017",
          "B17010_001","B17010_002","B25003_001","B25003_002","C17002_001",
          "C17002_002","C17002_003","C17002_004","C17002_005","B25044_001",
          "B25044_003","B25044_010","B25014_001","B25014_005","B25014_006",
          "B25014_007","B25014_011","B25014_012","B25014_013","B25088_001",
          "B25064_001","B25077_001","C24010_001","C24010_003","C24010_039",
          "B23025_001","B23025_005","B15003_001","B15003_002","B15003_003",
          "B15003_004","B15003_005","B15003_006","B15003_007","B15003_008",
          "B15003_009","B15003_010","B15003_011","B15003_012","B15003_017",
          "B15003_018","B15003_019","B15003_020","B15003_021","B15003_022",
          "B15003_023","B15003_024","B15003_025","B23008_001","B23008_008",
          "B23008_021"),
      ...)
  
  if(survey == "census") {
    if(is.null(year)) {
      tidycensus_args$year <- 2010
    }
    else if(!(year %in% c(1990, 2000, 2010))) {
      stop("To use decennial census data, specify year as 1990, 2000, or 2010")
    }
    tidycensus_function <- "tidycensus::get_decennial"
    tidycensus_args <-
      tidycensus_args[
        names(tidycensus_args) %in% formalArgs(tidycensus::get_decennial)]
  }
  else {
    if(is.null(year)) {
      tidycensus_args$year <- 2016
    }
    tidycensus_function <- "tidycensus::get_acs"
    tidycensus_args <-
      tidycensus_args[
        names(tidycensus_args) %in% formalArgs(tidycensus::get_acs)]
  }

  acs_adi <- calculate_adi(ref_area, tidycensus_function, tidycensus_args)

  return(acs_adi)
}
