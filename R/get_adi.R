#' Get area deprivation index (ADI)
#'
#' Returns the ADIs of user-specified areas.
#'
#' Returns a \code{tibble} or \code{sf tibble} of the area deprivation indices
#' (ADIs) of user-specified locations in the United States, utilizing US Census
#' data.
#'
#' The returned \code{tibble} or \code{sf tibble} is also of class \code{adi},
#' and it contains an attribute called \code{loadings}, which contains a named
#' numeric vector of the PCA loadings of each factor. This is accessible through
#' \code{attr(name_of_tibble, "loadings")}.
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
#'   A string under 5 digits long will yield all ZCTAs that begin with those
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
#'   dataset = "decennial" is currently under development.
#'
#'   Important: data not always available depending on the level of geography
#'   and data set chosen. See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html}.
#' @param geometry Logical value indicating whether or not shapefile data should
#'   be included in the result, making the result an \code{sf tibble} instead of
#'   a plain \code{tibble}. Defaults to \code{TRUE}.
#' @param shift_geo Logical value. See the \code{shift_geo} argument of
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} for details.
#' @param keep_indicators Logical value indicating whether or not the resulting
#'   \code{tibble} or \code{sf tibble} will contain the socioeconomic measures
#'   used to calculate the ADI values. Defaults to \code{FALSE}.
#' @param cache_tables The plural version of the \code{cache_table} argument in
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. (\code{get_adi()}
#'   calls the necessary \code{tidycensus} function many times in order to
#'   return ADIs, so many tables are cached if \code{TRUE}).
#' @param key Your Census API key as a character string. Obtain one at
#'   http://api.census.gov/data/key_signup.html. Defaults to \code{NULL}. Not
#'   necessary if you have already loaded your key with
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
#' @section Default behaviors: If \code{geography} is specified but
#'   \code{state}, \code{county}, \code{geoid}, and \code{zcta} are all left
#'   blank, the function will use the entire US (the 50 states plus the District
#'   of Columbia (DC) and Puerto Rico (PR)) as the reference area (see
#'   "Reference Area" above). Beware that this will take a long time if you set
#'   \code{geography = "tract"} or especially if \code{geography = "block
#'   group"}.
#'
#' @section The \code{geoid} parameter: Elements of \code{geoid} can represent
#'   different levels of geography, but they all must be either 2 digits (for
#'   states), 5 digits (for counties), 11 digits (for tracts), 12 digits (for
#'   block groups), or 15 digits (for blocks). It must contain character
#'   strings, so use quotation marks as well as leading zeros where applicable.
#'
#' @section Error handling: Depending on user input, this function may call its
#'   underlying functions (\code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}) many times in order
#'   to accommodate their behavior. If any of these calls are unsuccessful,
#'   \code{get_adi()} will try them again. The function quits if it attempts a
#'   batch of calls ten times without progress.
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
#' get_adi(geography = "tract", state = "OH", county = "Cuyahoga")
#'
#' get_adi(geography = "county", state = "CT", year = 2015, survey = "acs1",
#'         geometry = FALSE)
#'
#' delmarva_geoids <- c("10", "51001", "51131", "24015", "24029", "24035",
#'                      "24011", "24041", "24019", "24045", "24039", "24047")
#' delmarva <- get_adi(geography = "tract", geoid = delmarva_geoids)
#'
#' # Demonstration of geom_sf integration:
#' library(ggplot2)
#'
#' delmarva %>% ggplot() + geom_sf(aes(fill = ADI))
#' }
#' @return If \code{geometry = TRUE} (the default), an \code{sf tibble}. If
#'   \code{geometry = FALSE} is specified, a plain \code{tibble}.
#'
#'   If the census data contained too many missing values for imputation to take
#'   place, a \code{tibble} of the factors that could not undergo imputation,
#'   followed by the raw census data.
#' @export
get_adi <- function(geography,
                    state           = NULL,
                    county          = NULL,
                    geoid           = NULL,
                    zcta            = NULL,
                    year            = 2017,
                    dataset         = c("acs5", "acs3", "acs1", "decennial"),
                    geometry        = TRUE,
                    shift_geo       = FALSE,
                    keep_indicators = FALSE,
                    raw_data_only   = FALSE,
                    cache_tables    = TRUE,
                    key             = NULL,
                    ...) {
  
  geography <- validate_geography(geography)
  
  dataset <- validate_dataset(dataset, year, geography)
  
  partial_call <-
    partial_tidycensus_call(
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
      partial_call
    )
  
  if (geometry) {
    # Saves old tigris_use_cache value and puts it back when function exits
    old <- options(tigris_use_cache = TRUE)
    on.exit(options(old), add = TRUE)
  }
  
  census_data <-
    get_tidycensus(partial_call, state_county = ref_area$state_county)
  
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



partial_tidycensus_call <- function(dataset, year, ...) {
  
  if (dataset == "decennial") {
    .fn  <- tidycensus::get_decennial
    args <-
      rlang::dots_list(
        variables = choose_decennial_variables(year),
        sumfile = "sf3",
        year = year,
        table = NULL,
        output = "wide",
        ...,
        .homonyms = "first"
      )
  } else {
    .fn  <- tidycensus::get_acs
    args <-
      rlang::dots_list(
        variables = choose_acs_variables(year, dataset),
        survey = dataset,
        year = year,
        table = NULL,
        output = "wide",
        ...,
        .homonyms = "first"
      )
  }
  
  rlang::call_standardise(rlang::call2(.fn, !!!args))
}



get_tidycensus <- function(partial_call, state_county, ...) {
  
  message(length(state_county), " call(s) to tidycensus remaining.")
  
  d <- 
    lapply(
      state_county,
      function(sc) {
        tidycensus_call_message(sc)
        try(
          partial_call %>% rlang::call_modify(!!!sc) %>% eval(),
          silent = FALSE
        )
      }
    )
  
  error_indices <- 
    d %>% 
    vapply(inherits, FUN.VALUE = logical(1L), what = "try-error") %>% 
    which()
  
  if (length(error_indices) > 0L) {
    give_up_counter <- 1L
    
    while (length(error_indices) > 0L) {
      
      message("Retrying ", length(error_indices), " call(s) to tidycensus.")
      
      for (i in error_indices) {
        tidycensus_call_message(state_county[[i]])
        d[[i]] <-
          try(
            partial_call %>%
              rlang::call_modify(!!!state_county[[i]]) %>%
              eval(),
            silent = TRUE
          )
      }
      
      previous_errors <- error_indices
      
      error_indices <- 
        d %>% 
        vapply(inherits, FUN.VALUE = logical(1L), what = "try-error") %>% 
        which()
      
      if (identical(previous_errors, error_indices)) {
        give_up_counter <- give_up_counter + 1L
        if (give_up_counter > 10L) {
          stop(
            "Maximum number of futile attempts (10) reached."
          )
        }
      }
    }
  }
  
  d %>% purrr::reduce(rbind)
}




tidycensus_call_message <- function(sc) {
  if (!is.null(sc$state)) {
    message("\nState: ", sc$state)
    if (!is.null(sc$county)) {
      message("  County: ", sc$county)
    }
  }
}



choose_decennial_variables <- function(year) {
  if (year == 1990) {
    decennial_vars_1990$variable
  } else {
    decennial_vars_2000$variable
  }
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
