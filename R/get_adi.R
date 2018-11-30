#' Get area deprivation index (ADI)
#'
#' Returns the ADIs of user-specified areas.
#'
#' Returns a tibble or sf tibble of the area deprivation indices (ADIs) of
#' user-specified locations in the United States, utilizing American Community
#' Survey data.
#'
#' The returned tibble or sf tibble is also of class \code{adi}, and it contains
#' an attribute called \code{loadings}, which contains a named numeric vector of
#' the PCA loadings of each factor. This is accessible through
#' \code{attr(name_of_tibble, "loadings")}.
#'
#' @param geography A character string denoting the level of census geography
#'   whose ADIs you'd like to obtain. Must be one of \code{c("state", "county",
#'   "tract", or "block group")}. Defaults to \code{NULL}. See details for
#'   default behaviors when this and other parameters are left blank.
#' @param state A vector of character strings specifying the state(s) whose ADI
#'   data you're requesting. Defaults to \code{NULL}. Can contain full state
#'   names, two-letter state abbreviations, or FIPS codes/GEOIDs (must be a
#'   string, so use quotation marks and leading zeros). Must contain exactly one
#'   state if the parameter \code{counties} is also used. Must be blank if
#'   \code{geoid} is used.
#' @param county A vector of character strings specifying the counties whose ADI
#'   data you're requesting. Defaults to \code{NULL}. County names and
#'   three-digit FIPS codes are accepted (must contain strings, so use quotation
#'   marks and leading zeros). All elements must be in the same state;
#'   therefore, if using \code{county}, \code{state} must contain exactly one
#'   value. Must be blank if \code{geoid} is used.
#' @param geoid A character vector of GEOIDs (use quotation marks and leading
#'   zeros). Defaults to \code{NULL}. Must be blank if \code{state} and/or
#'   \code{county} is used. Can contain different levels of geography (see
#'   details).
#' @param year Single integer specifying the year of the ACS survey to use.
#'   Defaults to 2016.
#' @param survey The data set used to calculate ADIs. Must be one of
#'   \code{c("acs5", "acs3", "acs1")}, denoting the 5-, 3-, and 1-year ACS
#'   estimates. Defaults to \code{"acs5."} Important: data not always available
#'   depending on the level of geography and data set chosen. See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html}.
#' @param geometry Logical value indicating whether or not shapefile data should
#'   be included in the result, making the result an sf tibble instead of a
#'   plain tibble. Defaults to \code{TRUE}.
#' @param shift_geo Logical value. See the \code{shift_geo} argument of
#'   \code{\link[tidycensus]{get_acs}} for details.
#' @param keep_indicators Logical value indicating whether or not the resulting
#'   tibble or sf tibble will contain the socioeconomic measures used to
#'   calculate the ADI values. Defaults to \code{FALSE}.
#' @param key Your Census API key as a character string. Obtain one at
#'   http://api.census.gov/data/key_signup.html. Defaults to \code{NULL}. Not
#'   necessary if you have already loaded your key with
#'   \code{\link{census_api_key}}.
#' @param ... Additional arguments to be passed onto
#'   \code{tidycensus::get_acs()}.
#'
#' @section Reference area: \strong{The concept of "reference area" is important
#'   to understand when using this function.} The algorithm that produced the
#'   original ADIs employs factor analysis. As a result, the ADI is a relative
#'   measure; the ADI of a particular location is dynamic, varying depending on
#'   which other locations were supplied to the algorithm. In other words,
#'   \strong{ADI will vary depending on the reference area you specify.}
#'
#'   For example, the ADI of Orange County, California is \emph{x} when
#'   calculated alongside all other counties in California, but it is \emph{y}
#'   when calculated alongside all counties in the US. The \code{get_adi()}
#'   function enables the user to define a \strong{reference area} by feeding a
#'   vector of GEOIDs to its \code{geoid} parameter (or alternatively for
#'   convenience, a vector of state and county names/abbreviations to
#'   \code{state} and \code{county}). The function then gathers data from those
#'   specified locations and performs calculations using their data alone.
#'
#' @section Default behaviors: If \code{geography} is specified but
#'   \code{state}, \code{county}, and \code{geoid} are all left blank, the
#'   function will use the entire US (the 50 states plus the District of
#'   Columbia (DC) and Puerto Rico (PR)) as the reference area (see "Reference
#'   Area" above). Beware that this will take a long time if you set
#'   \code{geography = "tract"} or especially if \code{geography = "block
#'   group"}.
#'
#'   If \code{geography} is left blank, the function will choose the most
#'   specific level of geography specified by the parameter(s) \code{state},
#'   \code{county}, and/or \code{geoid}.
#'
#'   If all these parameters are left blank, the function will report the ADIs
#'   of all census tracts in the United States (the 50 states plus the District
#'   of Columbia (DC) and Puerto Rico (PR)). Beware: this takes a long time.
#'
#' @section The \code{geoid} parameter: Elements of \code{geoid} can represent
#'   different levels of geography, but they all must be either 2 digits (for
#'   states), 5 digits (for counties), 11 digits (for tracts) or 12 digits (for
#'   block groups). It must contain character strings, so use quotation marks as
#'   well as leading zero where applicable.
#'
#' @section Warnings and disclaimers: Please note that this function calls data
#'   from US Census servers, so execution may take a long time depending on the
#'   user's internet connection and the amount of data requested.
#'
#'   If there are any missing values, single imputation will be attempted using
#'   the \code{mice} package. Because of how \code{mice} is coded, the user must
#'   attach either the \code{sociome} package or the \code{mice} package for
#'   imputation to work (e.g., run \code{library("sociome")} or
#'   \code{library("mice")} before running \code{get_adi}). See
#'   \code{\link{mice.impute.pmm}} for details.
#'
#'   In the same vein, while this function allows flexibility in specifying
#'   reference areas (see the "Reference area" section above), data from the ACS
#'   are masked for sparsely populated places and may have too many missing
#'   values to return ADIs in some cases.
#'
#'   For advanced users, if changing the \code{survey} argument, be sure to know
#'   the advantages and limitations of the 1-year and 3-year ACS estimates. See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html.}
#'   for details.
#'
#' @seealso calculate_adi
#'
#' @examples
#' \dontrun{
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
#' }
#'
#' @return If \code{geometry = TRUE} (the default), an sf tibble with four
#'   columns: \code{GEOID}, \code{NAME}, \code{ADI}, and \code{geometry}. If
#'   \code{geometry = FALSE} is specified, a plain tibble with only the first
#'   three columns mentioned above.
#' @importFrom rlang .data
#' @export
get_adi <- function(geography       = NULL,
                    state           = NULL,
                    county          = NULL,
                    geoid           = NULL,
                    year            = 2016,
                    dataset         = c("acs5", "acs3", "acs1", "sf3"),
                    geometry        = TRUE,
                    shift_geo       = FALSE,
                    keep_indicators = FALSE,
                    key             = NULL,
                    ...
) {
  
  geoid    <- validate_geoid(geoid, state, county, ...)
  
  ref_area <- get_reference_area(geoid, geography)
  
  data     <- match.arg(dataset)
  
  if(geometry) {
    # Saves old tigris_use_cache value and puts it back when function exits
    old <- options(tigris_use_cache = TRUE)
    on.exit(options(old), add = TRUE)
  }
  
  raw_data <- get_tidycensus(ref_area  = ref_area,
                             year      = year,
                             geometry  = geometry,
                             shift_geo = shift_geo,
                             key       = key,
                             dataset   = dataset,
                             ...)
  
  # Since the call (or calls) to tidycensus::get_acs() above usually gathers
  # data on more places than what the user specified, this pares the data frame
  # down to only include the user-specified reference area.
  ref_area_data <- dplyr::filter(raw_data, .data$GEOID %in% ref_area$ref_geoids)
  
  # Passes the filtered tidycensus::get_acs() tibble (or sf tibble) onto
  # calculate_adi(), which produces the tibble (or sf tibble) of ADIs
  acs_adi <- calculate_adi(data            = ref_area_data,
                           type            = dataset,
                           keep_indicators = keep_indicators)
  
  return(acs_adi)
}


get_tidycensus <- function(ref_area, year, geometry, shift_geo, key, dataset,
                           ...) {
  
  args <- list(geography   = ref_area$geography,
               variables   = NULL,
               cache_table = TRUE, 
               year        = year,
               output      = "wide",
               geometry    = geometry,
               shift_geo   = shift_geo,
               key         = key,
               survey      = dataset,
               sumfile     = dataset,
               ...)
  
  if(dataset == "sf3") {
    fn             <- tidycensus::get_decennial
    args$variables <- decennial_vars$variable
                        
  }
  else {
    fn             <- tidycensus::get_acs
    args$variables <- acs_vars$variable         
  }
  
  args <- validate_tidycensus_args(args, fn)
  
  call_tidycensus(fn           = fn,
                  args         = args,
                  state_county = ref_area$state_county)
}


call_tidycensus <- function(fn, args, state_county) {
  
  # tidycensus::get_acs() is called separately for each user-specified state or
  # set of states. purrr::reduce(rbind) puts the results into a single data
  # frame
  lapply(state_county,
         function(state_county) rlang::exec(fn, !!!args, !!!state_county)) %>%
    purrr::reduce(rbind)
}