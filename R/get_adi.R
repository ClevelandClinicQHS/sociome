#' get_adi
#'
#' Returns a tibble of user-specified locations in the United States along with
#' their area deprivation indices (ADIs), utilizing American Community Survey
#' data.
#'
#' @param geography a character string denoting the type of location whose ADIs
#'   you'd like to see (e.g., "region", "county", "block group"). Must be one of
#'   the keywords accepted by the tidycensus::get_acs() function (see
#'   \url{https://walkerke.github.io/tidycensus/articles/basic-usage.html#geography-in-tidycensus}
#'    for a full list of options).
#' @param year The year, or endyear, of the ACS sample used to calculate the ADI
#'   values. 2010 through 2016 are available. Defaults to 2016.
#' @param state The state for which you are requesting ADI data. Not applicable
#'   for certain values of `geography`, required for other values of
#'   `geography`, and optional for still other values of `geography` (see
#'   \url{https://walkerke.github.io/tidycensus/articles/basic-usage.html#geography-in-tidycensus}
#'    for more information). State names, postal codes, and FIPS codes are
#'   accepted. Defaults to NULL.
#' @param county The county for which you are requesting ADI data. County names
#'   and FIPS codes are accepted. Must be combined with a value supplied to
#'   'state'. Not applicable for some values of `geography`, optional for other
#'   values of `geography`, but never required (see
#'   \code{\link[tidycensus]{census_api_key}} for more information).
#'   https://walkerke.github.io/tidycensus/articles/basic-usage.html#geography-in-tidycensus
#'    for more information). Defaults to NULL.
#' @param geoids A character vector of GEOIDs. See details.
#' @param key Your Census API key. Obtain one at
#'   http://api.census.gov/data/key_signup.html. Not necessary if you have
#'   already loaded your key with the tidycensus::census_api_key() function. See
#' @param survey The ACS contains one-year, three-year, and five-year surveys
#'   expressed as "acs1", "acs3", and "acs5". The default selection is "acs5."
#'
#' @return A tibble of three columns: GEOID of location, Name of location, ADI
#'   of location
#'
#' @details Elements of `geoids` can represent different levels of geography,
#'   but they all must be either 2 digits (for states), 5 digits (for counties),
#'   11 digits (for tracts) or 12 digits (for block groups). Be sure to put a
#'   leading zero where applicable.
#'
#' @examples
#' get_adi(geography = "tract", year = 2015, state = "OH", county = "Cuyahoga")
#' get_adi(geography = "region")
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
