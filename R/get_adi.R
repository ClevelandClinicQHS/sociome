#' Returns the ADIs of user-specified areas.
#'
#' Returns a tibble of the area deprivation indices (ADIs) of user-specified
#' locations in the United States, utilizing American Community Survey data.
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
#' @param year Single integer specifying the year of the ACS survey to use. Defaults to 2016.
#' @param survey The data set used to calculate ADIs. Must be one of c("acs5",
#'   "acs3", "acs1"), denoting the 5-, 3-, and 1-year ACS estimates. Defaults to
#'   "acs5." Important: data not always available depending on the level of
#'   geography and data set chosen. See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html}.
#' @param geometry Logical value indicating whether or not shapefile data should
#'   be included in the tibble. Defaults to TRUE.
#' @param key Your Census API key as a character string. Obtain one at
#'   http://api.census.gov/data/key_signup.html. Defaults to null. Not necessary
#'   if you have already loaded your key with \code{\link{census_api_key}}.
#' @param ... Additional arguments to be passed onto
#'   \code{tidycensus::get_acs()}.
#'
#' @details The algorithm that produced the original ADIs employs factor
#'   analysis. As a result, the ADI is a relative measure; the ADI of a
#'   particular location is dynamic, varying depending on which other locations
#'   were supplied to the algorithm. In other words, ADI will vary depending on
#'   the reference area. For example, the ADI of Orange County, California is
#'   \emph{x} when calculated alongside all other counties in California, but it
#'   is \emph{y} when calculated alongside all counties in the US. The
#'   \code{get_adi()} function enables the user to define a reference area by
#'   feeding a vector of GEOIDs to its \code{geoid} parameter (or alternatively
#'   for convenience, a vector of state and county names/abbreviations to
#'   \code{state} and \code{county}). The function then gathers data from those
#'   specified locations and performs calculations using their data alone.
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
#'   Please note that this function calls data from US Census servers, so
#'   execution may take a long time depending on the user's internet connection
#'   and the amount of data requested.
#'   
#'   If there are any missing values, single imputation will be attempted using
#'   the \code{mice} package. Because of how \code{mice} is coded, the user must
#'   attach the \code{sociome} package or the \code{mice} package for imputation
#'   to work (i.e., run \code{library("sociome")} and/or \code{library("mice")}
#'   before running \code{calculate_adi}).
#'   
#'   In the same vein, while this function allows flexibility in specifying
#'   reference areas, data from the ACS are masked for sparsely populated places
#'   and may have too many missing values to return ADIs in some cases.
#'   
#'   For advanced users, if adding the \code{survey} argument to \code{get_adi}
#'   to be passed to \code{tidycensus::get_acs}, be sure to know the limitations
#'   of the 1-year and 3-year ACS estimates. See
#'   https://www.census.gov/programs-surveys/acs/guidance/estimates.html.
#'
#' @examples
#' library("sociome") # Needed for imputation. library("mice") is another option.
#' get_adi(geography = "tract", state = "OH", county = "Cuyahoga")
#' 
#' get_adi(geography = "county", state = "CT", year = 2015, survey = "acs1", geometry = FALSE)
#'
#' delmarva_geoids <- c("10", "51001", "51131", "24015", "24029", "24035",
#'                      "24011", "24041", "24019", "24045", "24039", "24047")
#' delmarva <- get_adi(geoid = delmarva_geoids)
#' 
#' # Demonstration of geom_sf integration:
#' library(ggplot2)
#' 
#' delmarva %>% ggplot() + geom_sf(aes(fill = ADI))
#'
#' @return A tibble with four columns: \code{GEOID}, \code{NAME}, \code{ADI},
#'   and \code{geometry} (which is left out if \code{geometry = FALSE} is
#'   specified).
#'
#' @export

get_adi <- function(geography = NULL,
                    state     = NULL,
                    county    = NULL,
                    geoid    = NULL,
                    year      = 2016,
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
    
    # Throws error if user supplies county, state, and geoid
    if(!is.null(geoid)) {
      stop("If supplying geoid, state and county must be NULL")
    }
    
    # Validates user-supplied state and coerces it into its two-digit GEOID
    state <- validate_state(state)
    
    # Validates user-supplied county values and populates geoid with the
    # counties' five-digit GEOIDs
    geoid <-
      unique(sapply(county,
                    function(x) {
                      paste0(state,
                             validate_county(state = state, county = x))}))
  }
  
  
  else if(!is.null(state)) {

    # Throws error if user supplies both states and geoid
    if(!is.null(geoid)) {
      stop("If supplying geoid, state and county must be NULL")
    }
    
    # Populates geoid with states' two-digit geoid.
    else {
      geoid <- unique(sapply(state, validate_state))
    }
    
  }
  
  # Sends geoid and geography to the function get_reference_area, which
  # validates the GEOIDs in geoid and returns a ref_area-class object, which
  # contains all the data needed to produce the ADIs specified by the user.
  ref_area <- get_reference_area(geoid, geography)
  
  # Makes a list of all arguments necessary for running tidycensus::get_acs,
  # with the exception of state and county. Notice that the dots (...) are
  # included in this list. This allows the user to pass other arguments to
  # tidycensus::get_acs() if desired (e.g., shift_geo).
  get_acs_args <-
    list(
      geography = ref_area$geography,
      year = year, survey = survey, geometry = geometry, key = key,
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
  
  # Validates the names of the elements of get_acs_args against the formalArgs()
  # of tidycensus::get_acs(), ensuring that only arguments useable by
  # tidycensus::get_acs() are included
  get_acs_args <- get_acs_args[names(get_acs_args) %in%
                                 methods::formalArgs(tidycensus::get_acs)]
  
  # Saves old tigris_use_cache value and puts it back when function exits
  old <- options(tigris_use_cache = TRUE)
  on.exit(options(old), add = TRUE)
  
  # purrr::map() is used to call tidycensus::get_acs() for each user-specified
  # state or set of user-specified states.
  # purrr::reduce(rbind) puts the results into a single data frame
  acs_data_raw <-
    ref_area$state_county %>% 
    purrr::map(
      function(state_county, get_acs_args) {
        state  <- state_county$state
        county <- state_county$county
        do.call(eval(parse(text = "tidycensus::get_acs")),
                c(list(state = state, county = county), get_acs_args))
      },
      get_acs_args = get_acs_args) %>%
    purrr::reduce(rbind)
  
  # Since the call (or calls) to tidycensus::get_acs() above usually gathers
  # data on more places than what the user specified, this pares the data frame
  # down to only include the user-specified reference area.
  acs_ref_area <- acs_data_raw %>%
    dplyr::filter(.data$GEOID %in% ref_area$ref_geoids)
  
  # Passes the filtered tidycensus::get_acs() tibble onto calculate_adi(), which
  # produces the tibble of ADIs
  acs_adi <- calculate_adi(acs_ref_area)

  return(acs_adi)
}


###############################################################################
# The functions below are copied verbatim or nearly verbatim from the tidycensus
# package's internal functions with the same names.
# Written by Kyle Walker.
###############################################################################
validate_state <- function(state, .msg = interactive()) 
{
  if (is.null(state)) 
    return(NULL)
  state <- tolower(stringr::str_trim(state))
  if (grepl("^[[:digit:]]+$", state)) {
    state <- sprintf("%02d", as.numeric(state))
    if (state %in% fips_state_table$fips) {
      return(state)
    }
    else {
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        message(
          sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                  state, state_sub,
                  fips_state_table[fips_state_table$fips == 
                                     state_sub, "name"]),
          call. = FALSE)
        return(state_sub)
      }
      else {
        warning(
          sprintf("'%s' is not a valid FIPS code or state name/abbreviation",
                  state), call. = FALSE)
        return(NULL)
      }
    }
  }
  else if (grepl("^[[:alpha:]]+", state)) {
    if (nchar(state) == 2 && state %in% fips_state_table$abb) {
      if (.msg) 
        message(sprintf("Using FIPS code '%s' for state '%s'", 
                        fips_state_table[fips_state_table$abb == state, 
                                         "fips"], toupper(state)))
      return(fips_state_table[fips_state_table$abb == state, 
                              "fips"])
    }
    else if (nchar(state) > 2 && state %in% fips_state_table$name) {
      if (.msg) 
        message(sprintf("Using FIPS code '%s' for state '%s'", 
                        fips_state_table[fips_state_table$name == state, 
                                         "fips"], simpleCapSO(state)))
      return(fips_state_table[fips_state_table$name == 
                                state, "fips"])
    }
    else {
      warning(
        sprintf("'%s' is not a valid FIPS code or state name/abbreviation", 
                      state), call. = FALSE)
      return(NULL)
    }
  }
  else {
    warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", 
                    state), call. = FALSE)
    return(NULL)
  }
}

simpleCapSO <- function(x) 
{
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ")
}

validate_county <- function (state, county, .msg = interactive()) 
{
  if (is.null(state) || is.null(county)) 
    return(NULL)
  state <- validate_state(state)
  county_table <-
    tidycensus::fips_codes[tidycensus::fips_codes$state_code == state, 
                             ]
  if (grepl("^[[:digit:]]+$", county)) {
    county <- sprintf("%03d", as.numeric(county))
    if (county %in% county_table$county_code) {
      return(county)
    }
    else {
      warning(sprintf("'%s' is not a valid FIPS code for counties in %s", 
                      county, county_table$state_name[1]), call. = FALSE)
      return(NULL)
    }
  }
  else if ((grepl("^[[:alpha:]]+", county))) {
    county_index <- grepl(sprintf("^%s", county), county_table$county, 
                          ignore.case = TRUE)
    matching_counties <- county_table$county[county_index]
    if (length(matching_counties) == 0) {
      warning(sprintf("'%s' is not a valid name for counties in %s", 
                      county, county_table$state_name[1]), call. = FALSE)
      return(NULL)
    }
    else if (length(matching_counties) == 1) {
      if (.msg) 
        message(sprintf("Using FIPS code '%s' for '%s'", 
                        county_table[county_table$county == matching_counties, 
                                     "county_code"], matching_counties))
      return(county_table[county_table$county == matching_counties, 
                          "county_code"])
    }
    else if (length(matching_counties) > 1) {
      ctys <- format_vec(matching_counties)
      warning("Your county string matches ", ctys,
              " Please refine your selection.", call. = FALSE)
      return(NULL)
    }
  }
}

format_vec <- function(vec) 
{
  out <- paste0(vec, ", ")
  l <- length(out)
  out[l - 1] <- paste0(out[l - 1], "and ")
  out[l] <- gsub(", ", ".", out[l])
  return(paste0(out, collapse = ""))
}