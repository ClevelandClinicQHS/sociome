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
                    state = NULL,
                    geoids = NULL,
                    year = 2016,
                    survey = "acs5",
                    key = NULL) {

  if(!is.null(state)) {

    # Throws error if user supplies both states and geoids
    if(!is.null(geoids)) {
      stop("Cannot supply both geoids and state.")
    }

    # If user supplied states instead of geoids, fills geoids with the states'
    # geoids.
    else {
      geoids <- unique(sapply(state, tidycensus:::validate_state))
    }
  }

  ref_area <- get_reference_area(geoids, geography)

  acs_adi <- calculate_adi(ref_area, year, survey, key)

  return(acs_adi)
}
