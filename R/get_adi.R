#' Get area deprivation index (ADI)
#'
#' Returns the ADIs of user-specified areas.
#'
#' Returns a \code{\link[tibble]{tibble}} or \code{\link[sf]{sf}} \code{tibble}
#' of the area deprivation indices (ADIs) of user-specified locations in the
#' United States, utilizing US Census data.
#'
#' The returned \code{\link[tibble]{tibble}} or \code{\link[sf]{sf}}
#' \code{tibble} is also of class \code{adi}, and it contains an attribute
#' called \code{loadings}, which contains a named numeric vector of the PCA
#' loadings of each factor. This is accessible through
#' \code{\link{attr}(name_of_tibble, "loadings")}.
#'
#' @param geography A character string denoting the level of census geography
#'   whose ADIs you'd like to obtain. Must be one of \code{c("state", "county",
#'   "tract", "block group", or "zcta")}. Required.
#' @param state A character string specifying states whose ADI data is desired.
#'   Defaults to \code{NULL}. Can contain full state names, two-letter state
#'   abbreviations, or a two-digit FIPS code/GEOID (must be a vector of strings,
#'   so use quotation marks and leading zeros if necessary). Must be left as
#'   \code{NULL} blank if using the \code{geoid} or \code{zcta} parameter.
#' @param county A vector of character strings specifying the counties whose ADI
#'   data you're requesting. Defaults to \code{NULL}. If not \code{NULL}, the
#'   \code{state} parameter must have a length of 1. County names and
#'   three-digit FIPS codes are accepted (must contain strings, so use quotation
#'   marks and leading zeros if necessary). Must be blank if using the
#'   \code{geoid} parameter.
#' @param geoid A character vector of GEOIDs (use quotation marks and leading
#'   zeros). Defaults to \code{NULL}. Must be blank if \code{state},
#'   \code{county}, or \code{zcta} is used. Can contain different levels of
#'   geography (see details).
#' @param zcta A character vector of ZCTAs or the leading digit(s) of ZCTAs (use
#'   quotation marks and leading zeros). Defaults to \code{NULL}. Must be blank
#'   if \code{state}, \code{county}, or \code{geoid} is used.
#'
#'   Strings under 5 digits long will yield all ZCTAs that begin with those
#'   digits.
#'
#'   Requires that \code{geography = "zcta"}. If \code{geography = "zcta"} and
#'   \code{zcta = NULL}, all ZCTAs in the US will be used.
#' @param year Single integer specifying the year of US Census data to use.
#'   Defaults to 2017.
#' @param dataset The data set used to calculate ADIs. Must be one of
#'   \code{c("acs5", "acs3", "acs1", "decennial")}, denoting the 5-, 3-, and
#'   1-year ACS along with the decennial census. Defaults to \code{"acs5"}.
#'
#'   When \code{dataset = "decennial"}, \code{year} must be in \code{c(1990,
#'   2000, 2010)}.
#'
#'   The 2010 decennial census did not include the long-form questionnaire used
#'   in the 1990 and 2000 censuses, so this function uses the 5-year estimates
#'   from the 2010 ACS to supply the data not included in the 2010 decennial
#'   census. In fact, the only 2010 decennial variables used are H003002,
#'   H014002, P020002, and P020008.
#'
#'   Important: data are not always available depending on the level of
#'   geography and data set chosen. See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html}.
#' @param geometry Logical value indicating whether or not shapefile data should
#'   be included in the result, making the result an \code{\link[sf]{sf}}
#'   \code{tibble} instead of a plain \code{\link[tibble]{tibble}}. Defaults to
#'   \code{FALSE}.
#' @param shift_geo Logical value. See the \code{shift_geo} argument of
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()} for details.
#' @param keep_indicators Logical value indicating whether or not the resulting
#'   \code{\link[tibble]{tibble}} or \code{\link[sf]{sf}} \code{tibble} will
#'   contain the socioeconomic measures used to calculate the ADI values.
#'   Defaults to \code{FALSE}.
#'   
#'   See \code{\link{acs_vars}} and \code{\link{decennial_vars}} for basic
#'   descriptions of the raw census variables.
#' @param cache_tables The plural version of the \code{cache_table} argument in
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. (\code{get_adi()}
#'   calls the necessary \code{tidycensus} function many times in order to
#'   return ADIs, so many tables are cached if \code{TRUE}). Defaults to
#'   \code{TRUE}.
#' @param key Your Census API key as a character string. Obtain one at
#'   \url{http://api.census.gov/data/key_signup.html}. Defaults to \code{NULL}.
#'   Not necessary if you have already loaded your key with
#'   \code{\link{census_api_key}()}.
#' @param raw_data_only Logical, indicating whether or not to skip calculation
#'   of the ADI and only return the census variables. Defaults to \code{FALSE}.
#' @param ... Additional arguments to be passed onto
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}.
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
#'   convenience, a state and/or counties to \code{state} and \code{county}).
#'   The function then gathers data from those specified locations and performs
#'   calculations using their data alone.
#'
#' @section The \code{geoid} parameter: Elements of \code{geoid} can represent
#'   different levels of geography, but they all must be either 2 digits (for
#'   states), 5 digits (for counties), 11 digits (for tracts), or 12 digits (for
#'   block groups). It must contain character strings, so use quotation marks as
#'   well as leading zeros where applicable.
#'
#' @section Error handling: Depending on user input, this function may call its
#'   underlying functions (\code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}) many times in order
#'   to accommodate their behavior. These calls are wrapped in
#'   \code{purrr::\link[purrr]{insistently}(rate =
#'   purrr::\link[purrr]{rate_delay}())}.
#'
#' @section Warnings and disclaimers: Please note that this function calls data
#'   from US Census servers, so execution may take a long time depending on the
#'   user's internet connection and the amount of data requested.
#'
#'   If there are any missing values, single imputation will be attempted using
#'   the \code{mice} package.
#'
#'   In the same vein, while this function allows flexibility in specifying
#'   reference areas (see the \strong{Reference area} section above), data from
#'   the US Census are masked for sparsely populated places and may have too
#'   many missing values to return ADIs in some cases.
#'
#'   For advanced users, if changing the \code{dataset} argument, be sure to
#'   know the advantages and limitations of the 1-year and 3-year ACS estimates.
#'   See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html.}
#'   for details.
#'
#' @examples
#' \donttest{
#' # Wrapped in \donttest{} because all these examples take >5 seconds
#' # and require a Census API key.
#'
#' # ADI of all census tracts in Cuyahoga County, Ohio
#' get_adi(geography = "tract", state = "OH", county = "Cuyahoga")
#'
#' # ADI of all counties in Connecticut.
#' # Returns a warning because there are only 8 counties.
#' # A minimum of 30 locations is recommended.
#' get_adi(geography = "county", state = "CT", year = 2015, dataset = "acs1")
#'
#' # ADI of all census tracts in the GEOIDs below.
#' # Notice the mixing of state- ("10") and county-level GEOIDs (the others).
#' delmarva_geoids <- c("10", "51001", "51131", "24015", "24029", "24035",
#'                      "24011", "24041", "24019", "24045", "24039", "24047")
#' delmarva <-
#'   get_adi(
#'     geography = "tract",
#'     geoid = delmarva_geoids,
#'     dataset = "decennial",
#'     year = 2000,
#'     geometry = TRUE
#'   )
#'
#' # Demonstration of geom_sf integration:
#' require(ggplot2)
#'
#' delmarva %>% ggplot() + geom_sf(aes(fill = ADI))
#'
#' # Return the loadings of the indicators used to calculate the delmarva ADIs
#' attr(delmarva, "loadings")
#' }
#' @return If \code{geometry = FALSE}, (the default) a
#'   \code{\link[tibble]{tibble}}. If \code{geometry = TRUE} is specified, an
#'   \code{\link[sf]{sf}} \code{tibble}.
#'
#'   If the census data contained too many missing values for imputation to take
#'   place, a \code{\link[tibble]{tibble}} of the factors that could not undergo
#'   imputation, followed by the raw census data.
#' @export
get_adi <- function(geography,
                    state           = NULL,
                    county          = NULL,
                    geoid           = NULL,
                    zcta            = NULL,
                    year            = 2017,
                    dataset         = c("acs5", "acs3", "acs1", "decennial"),
                    geometry        = FALSE,
                    shift_geo       = FALSE,
                    keep_indicators = FALSE,
                    raw_data_only   = FALSE,
                    cache_tables    = TRUE,
                    key             = NULL,
                    ...) {
  
  geography <- validate_geography(geography)
  year      <- validate_year(year)
  dataset   <- validate_dataset(dataset, year, geography)
  
  exec_arg_tibble <-
    exec_arg_tibble(
      dataset,
      year, 
      geography = geography,
      geometry = geometry,
      shift_geo = shift_geo,
      cache_table = cache_tables,
      key = key,
      ...
    )
  
  ref_area <-
    validate_location(
      geoid, 
      state, 
      county, 
      zcta, 
      geography, 
      dataset,
      exec_arg_tibble
    )
  
  exec_arg_tibble <- tidyr::crossing(exec_arg_tibble, ref_area$state_county)
  
  if (geometry) {
    # Saves old tigris_use_cache value and puts it back when function exits
    old <- options(tigris_use_cache = TRUE)
    on.exit(options(old), add = TRUE)
  }
  
  census_data <- get_tidycensus(exec_arg_tibble)
  
  # Since the call (or calls) to tidycensus functions usually gathers data on
  # more places than what the user specified, this pares the data frame down to
  # only include the user-specified reference area.
  if (!is.null(ref_area$geoid)) {
    census_data <-
      filter_ref_area(
        data       = census_data,
        what       = "GEOID",
        pattern    = ref_area$geoid,
        geo_length = ref_area$geo_length
      )
  } else if (!is.null(ref_area$zcta)) {
    census_data <-
      filter_ref_area(
        data       = census_data,
        what       = "ZCTA",
        pattern    = ref_area$zcta
      )
  }
  
  if (raw_data_only) {
    census_data
  } else {
    calculate_adi(census_data, keep_indicators = keep_indicators)
  }
}



#' @importFrom rlang .data
exec_arg_tibble <- function(dataset, year, ...) {
  
  dots <- lapply(list(...), list)
  
  if (dataset == "decennial") {
    
    if (year == 2010) {
      
      variables <- 
        list(
          sociome::decennial_vars %>% 
            dplyr::filter(.data$year == 2010) %>% 
            dplyr::pull("variable"),
          sociome::acs_vars %>% 
            dplyr::filter(.data$decennial2010) %>% 
            dplyr::pull("variable")
        )
      
      rlang::dots_list(
        .fn = list(tidycensus::get_decennial, tidycensus::get_acs),
        variables = variables,
        sumfile = list("sf1", NULL),
        year = year,
        output = "tidy",
        !!!dots,
        .homonyms = "first"
      ) %>% 
        tibble::as_tibble(.rows = 2L)
      
    } else {
      variables <- 
        sociome::decennial_vars %>%
        dplyr::filter(.data$year == !!year) %>% 
        split(.$sumfile) %>% 
        lapply(dplyr::pull, var = "variable")
      
      rlang::dots_list(
        .fn = list(tidycensus::get_decennial),
        variables = variables,
        sumfile = c("sf1", "sf3"),
        year = year,
        output = "tidy",
        !!!dots,
        .homonyms = "first"
      ) %>% 
        tibble::as_tibble(.rows = 2L)
    }
    
  } else {
    rlang::dots_list(
      .fn = list(tidycensus::get_acs),
      variables = list(choose_acs_variables(year = year, dataset = dataset)),
      year = year,
      survey = dataset,
      output = "tidy",
      !!!dots,
      .homonyms = "first"
    ) %>% 
      tibble::as_tibble(.rows = 1L)
  }
}


#' @importFrom rlang .data
get_tidycensus <- function(exec_arg_tibble) {
  
  message(nrow(exec_arg_tibble), " call(s) to tidycensus beginning.")
  
  result <-
    exec_arg_tibble %>% 
    purrr::pmap(exec_insistently) %>% 
    lapply(dplyr::select, "GEOID", "NAME", "key" = 3L, "value" = 4L) %>% 
    Reduce(f = rbind)
  
  geoid_match <- result$GEOID %>% match(., .)
  
  result$NAME <- result$NAME[geoid_match]
  
  if (any(colnames(result) == "geometry")) {
    result$geometry <- result$geometry[geoid_match]
  }
  
  tidyr::spread(result, key = "key", value = "value")
}



#' @importFrom rlang .data
choose_acs_variables <- function(year, dataset) {
  
  if (year > 2010) {
    if (year == 2011 && dataset == "acs5") {
      sociome::acs_vars %>% 
        dplyr::filter(.data$B23025_and_B15002) %>% 
        dplyr::pull("variable")
    } else {
      sociome::acs_vars %>% 
        dplyr::filter(.data$B23025_and_B15003) %>% 
        dplyr::pull("variable")
    }
  } else if (
    dataset == "acs1" && year > 2007 || dataset == "acs3" && year == 2010
  ) {
    sociome::acs_vars %>% 
      dplyr::filter(.data$B23001_and_B15003) %>% 
      dplyr::pull("variable")
  } else {
    sociome::acs_vars %>% 
      dplyr::filter(.data$B23001_and_B15002) %>% 
      dplyr::pull("variable")
  }
}



exec_insistently <- purrr::insistently(rlang::exec, rate = purrr::rate_delay())



filter_ref_area <- function(data,
                            what,
                            pattern,
                            geo_length = NULL) {
  
  if (is.null(geo_length)) {
    pattern_sub <- pattern
  } else {
    pattern_sub <- stringr::str_sub(pattern, 1L, geo_length)
  } 
  
  matches <-
    lapply(
      paste0("^", pattern_sub),
      stringr::str_which,
      string = data$GEOID
    )
  
  nomatch <- lapply(matches, length) == 0L
  
  if (any(nomatch)) {
    warning(
      "The following ", what, "s had no match in census data:\n",
      paste(pattern[nomatch], collapse = ",\n")
    )
  }
  
  matches <- unique(unlist(matches, use.names = FALSE))
  
  data[matches, ]
}
