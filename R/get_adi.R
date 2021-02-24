#' Get Area Deprivation Index (ADI) and Berg Indices (ADI-3)
#'
#' Returns the ADI and ADI-3 of user-specified areas.
#'
#' Returns a \code{\link[tibble]{tibble}} or \code{\link[sf]{sf}} \code{tibble}
#' of the Area Deprivation Indices (ADIs) and Berg Indices (ADI-3s) of
#' user-specified locations in the United States, utilizing US Census data.
#' Locations that are listed as having zero households are excluded from ADI and
#' ADI-3 calculation: their ADI and ADI-3 values will be \code{NA}.
#'
#' @param geography A character string denoting the level of census geography
#'   whose ADIs and ADI-3s you'd like to obtain. Must be one of \code{c("state",
#'   "county", "tract", "block group", or "zcta")}. Required.
#' @param state A character string specifying states whose ADI and ADI-3 data is
#'   desired. Defaults to \code{NULL}. Can contain full state names, two-letter
#'   state abbreviations, or a two-digit FIPS code/GEOID (must be a vector of
#'   strings, so use quotation marks and leading zeros if necessary). Must be
#'   left as \code{NULL} blank if using the \code{geoid} or \code{zcta}
#'   parameter.
#' @param county A vector of character strings specifying the counties whose ADI
#'   and ADI-3 data you're requesting. Defaults to \code{NULL}. If not
#'   \code{NULL}, the \code{state} parameter must have a length of 1. County
#'   names and three-digit FIPS codes are accepted (must contain strings, so use
#'   quotation marks and leading zeros if necessary). Must be blank if using the
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
#' @param dataset The data set used to calculate ADIs and ADI-3s. Must be one of
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
#'
#'   The shapefile data that is returned is somewhat customizable: see the
#'   \code{shift_geo} and \code{...} arguments.
#' @param shift_geo Logical value. See the \code{shift_geo} argument of
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()} for details.
#'
#'   See \code{...} below for other ways to customize the shapefile data
#'   returned.
#' @param keep_indicators Logical value indicating whether or not the resulting
#'   \code{\link[tibble]{tibble}} or \code{\link[sf]{sf}} \code{tibble} will
#'   contain the socioeconomic measures used to calculate the ADI and ADI-3
#'   values. Defaults to \code{FALSE}.
#'
#'   See \code{\link{acs_vars}} and \code{\link{decennial_vars}} for basic
#'   descriptions of the raw census variables.
#' @param cache_tables The plural version of the \code{cache_table} argument in
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. (\code{get_adi()}
#'   calls the necessary \code{tidycensus} function many times in order to
#'   return ADIs and ADI-3s, so many tables are cached if \code{TRUE}). Defaults
#'   to \code{TRUE}.
#' @param key Your Census API key as a character string. Obtain one at
#'   \url{http://api.census.gov/data/key_signup.html}. Defaults to \code{NULL}.
#'   Not necessary if you have already loaded your key with
#'   \code{\link{census_api_key}()}.
#' @param raw_data_only Logical, indicating whether or not to skip calculation
#'   of the ADI and ADI-3 and only return the census variables. Defaults to
#'   \code{FALSE}.
#' @param seed Passed to \code{\link{calculate_adi}()}.
#' @param ... Additional arguments to be passed onto
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}. Currently, none of
#'   these functions' formal arguments can be meaningfully customized (doing so
#'   will either throw an error or have no effect). However, when setting
#'   \code{geometry = TRUE}, the \code{tidycensus} functions do pass meaningful
#'   arguments onto the appropriate \code{tigris} function (namely, one of
#'   \code{\link[tigris]{states}()}, \code{\link[tigris]{counties}()},
#'   \code{\link[tigris]{tracts}()}, \code{\link[tigris]{block_groups}()}, or
#'   \code{\link[tigris]{zctas}()}, according to the the value of
#'   \code{geography}). This enables the user to somewhat customize the
#'   shapefile data obtained.
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
#'   convenience, states and/or counties to \code{state} and \code{county}). The
#'   function then gathers data from those specified locations and performs
#'   calculations using their data alone.
#'
#'   The Berg Indices (ADI-3) were developed with this principle of relativity
#'   in mind, and as such there is no set of seminal ADI-3 values. Thus, the
#'   terms "Berg Indices" and "ADI-3" refer more nearly to any values generated
#'   using the algorithm employed in this package.
#'
#'   Areas listed as having zero households are excluded from the reference
#'   area, and their ADI and ADI-3 values will be \code{NA}.
#'
#' @section The \code{geoid} parameter: Elements of \code{geoid} can represent
#'   different levels of geography, but they all must be either 2 digits (for
#'   states), 5 digits (for counties), 11 digits (for tracts), or 12 digits (for
#'   block groups). It must contain character strings, so use quotation marks as
#'   well as leading zeros where applicable.
#'
#' @section ADI and ADI-3 factor loadings: The returned
#'   \code{\link[tibble]{tibble}} or \code{\link[sf]{sf}} \code{tibble} is of
#'   class \code{adi}, and it contains an attribute called \code{loadings},
#'   which contains a tibble of the PCA loadings of each factor. This is
#'   accessible through \code{\link{attr}(name_of_tibble, "loadings")}.
#'
#' @section Missingness and imputation: While this function allows flexibility
#'   in specifying reference areas (see the \strong{Reference area} section
#'   above), data from the US Census are masked for sparsely populated places,
#'   resulting in many missing values.
#'
#'   Imputation is attempted via \code{mice::\link[mice]{mice}(m = 1, maxit =
#'   50, method = "pmm", seed = seed)}. If imputation is unsuccessful, an error
#'   is thrown, but the dataset of indicators on which imputation was
#'   unsuccessful is available via
#'   \code{rlang::\link[rlang]{last_error}()$adi_indicators} and the raw census
#'   data are available via
#'   \code{rlang::\link[rlang]{last_error}()$adi_raw_data}. The former excludes
#'   areas with zero households, but the latter includes them.
#'
#'   One of the indicators of both ADI and the Financial Strength component of
#'   ADI-3 is median family income, but methodological issues with the 2015 and
#'   2016 ACS have rendered this variable unavailable at the block group level
#'   for those years. When requested, this function will use median household
#'   income in its place, with a \code{warning()}. See
#'   \url{https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2016-01.html}.
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' @section API-related error handling: Depending on user input, this function
#'   may call its underlying functions
#'   (\code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}) many times in order
#'   to accommodate their behavior. When these calls are broken up by state or
#'   by state and county, a message is printed indicating the state or state and
#'   county whose data is being pulled. These calls are wrapped in
#'   \code{purrr::\link[purrr]{insistently}(rate =
#'   purrr::\link[purrr:rate-helpers]{rate_delay}(), quiet = FALSE)}, meaning
#'   that they are attempted over and over until success, and \code{tidycensus}
#'   error messages are printed as they occur.
#'
#' @section Warnings and disclaimers: Please note that this function calls data
#'   from US Census servers, so execution may take a long time depending on the
#'   user's internet connection and the amount of data requested.
#'
#'   For advanced users, if changing the \code{dataset} argument, be sure to
#'   know the advantages and limitations of the 1-year and 3-year ACS estimates.
#'   See
#'   \url{https://www.census.gov/programs-surveys/acs/guidance/estimates.html.}
#'   for details.
#'
#' @examples
#' \dontrun{
#' # Wrapped in \dontrun{} because all these examples take >5 seconds
#' # and require a Census API key.
#'
#' # ADI of all census tracts in Cuyahoga County, Ohio
#' get_adi(geography = "tract", year = 2017, state = "OH", county = "Cuyahoga")
#'
#' # ADI and ADI-3 of all counties in Connecticut, using the 2014 ACS1 survey.
#' # Returns a warning because there are only 8 counties.
#' # A minimum of 30 locations is recommended.
#' get_adi(geography = "county", state = "CT", year = 2014, dataset = "acs1")
#'
#' # Areas with zero households will have an ADI and ADI-3 of NA:
#' queens <-
#'   get_adi(
#'     "tract",
#'     year = 2017,
#'     state = "NY",
#'     county = "Queens",
#'     keep_indicators = TRUE,
#'     geometry = TRUE
#'   )
#' queens %>%
#'   dplyr::as_tibble() %>%
#'   dplyr::select(GEOID, NAME, ADI, households = B11005_001) %>%
#'   dplyr::filter(is.na(ADI) | households == 0) %>%
#'   print(n = Inf)
#'
#' # geoid argument allows for highly customized reference populations.
#' # ADI of all census tracts in the GEOIDs stored in "delmarva" below:
#' # Notice the mixing of state- ("10") and county-level GEOIDs (the others).
#' delmarva_geoids <- c("10", "51001", "51131", "24015", "24029", "24035",
#'                      "24011", "24041", "24019", "24045", "24039", "24047")
#' delmarva <-
#'   get_adi(
#'     geography = "tract",
#'     geoid = delmarva_geoids,
#'     dataset = "acs5",
#'     year = 2009,
#'     geometry = TRUE
#'   )
#'
#' # Demonstration of geom_sf() integration:
#' require(ggplot2)
#'
#' # The na.value argument changes the fill of NA ADI areas.
#' delmarva %>% ggplot() + geom_sf(aes(fill = ADI), lwd = 0)
#'
#' # Setting direction = -1 makes the less deprived areas the lighter ones
#' # The argument na.value changes the color of zero-household areas
#' queens %>%
#'   ggplot() +
#'   geom_sf(aes(fill = ADI), lwd = 0) +
#'   scale_fill_viridis_c(na.value = "red", direction = -1)
#'
#' # Obtain factor loadings:
#' attr(queens, "loadings")
#' }
#' @return If \code{geometry = FALSE}, (the default) a
#'   \code{\link[tibble]{tibble}}. If \code{geometry = TRUE} is specified, an
#'   \code{\link[sf]{sf}} \code{tibble}.
#' @export
get_adi <- function(geography,
                    state           = NULL,
                    county          = NULL,
                    geoid           = NULL,
                    zcta            = NULL,
                    year,
                    dataset         = c("acs5", "acs3", "acs1", "decennial"),
                    geometry        = FALSE,
                    shift_geo       = FALSE,
                    keep_indicators = FALSE,
                    raw_data_only   = FALSE,
                    cache_tables    = TRUE,
                    key             = NULL,
                    seed            = NA,
                    ...) {
  
  geography <- validate_geography(geography)
  year      <- validate_year(year)
  dataset   <- validate_dataset(dataset, year, geography)
  
  # Any given call to get_adi() necessitates one or more calls to
  # tidycensus::get_acs() and/or tidycensus::get_decennial(). This function
  # creates the skeletons of these calls, to be completed later according to the
  # reference area.
  tidycensus_calls <-
    make_tidycensus_calls(
      dataset,
      year, 
      geography,
      geometry = geometry,
      shift_geo = shift_geo,
      cache_table = cache_tables,
      key = key,
      ...
    )
  
  # This analyzes the inputs so that the proper reference area is used in the
  # calculation of ADI and ADI-3.
  ref_area <-
    validate_location(
      geoid, 
      state, 
      county, 
      zcta, 
      geography, 
      year,
      dataset,
      tidycensus_calls
    )
  
  if (geometry) {
    # Saves old tigris_use_cache value and puts it back when function exits
    old <- options(tigris_use_cache = TRUE)
    on.exit(options(old), add = TRUE)
  }
  
  # Generally, the smaller the level of geography requested, the smaller the
  # geographical scope that tidycensus::get_acs() and
  # tidycensus::get_decennial() allow. Therefore, the tidycensus function(s) may
  # have to be called multiple times, breaking up the requested geographical
  # area by state and/or county. This function breaks these calls up as needed
  # and evaluates them.
  census_data <-
    get_tidycensus(
      tidycensus_calls = tidycensus_calls,
      state_county = ref_area$state_county,
      geography = geography,
      year = year,
      dataset = dataset
    )
  
  # Since the call (or calls) to tidycensus functions usually gathers data on
  # more places than what the user specified, this pares the tidycensus-produced
  # data down to only include the user-specified reference area.
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
    calculate_adi(census_data, keep_indicators = keep_indicators, seed = seed)
  }
}



#' @importFrom rlang .data
make_tidycensus_calls <- function(dataset, year, geography, ...) {
  
  if (dataset == "decennial") {
    
    # There is no survey argument in tidycensus::get_decennial()
    survey <- list(rlang::zap())
    
    # The 2010 decennial census did not gather the same detailed data that the
    # 2000 and 1990 censuses did (i.e., the 2010 census has no SF3), so its
    # gaps are filled with the 2010 5-year ACS estimates (in reality, the
    # majority of the data come from the ACS rather than the 2010 Census).
    if (year == 2010) {
      
      # To be fed to the .call argument of rlang::call_modify()
      .call <- alist(tidycensus::get_decennial(), tidycensus::get_acs())
      variables <- 
        list(
          dplyr::filter(sociome::decennial_vars, .data$year == 2010)$variable,
          dplyr::filter(sociome::acs_vars, .data$dec2010)$variable
        )
      
      # rlang::zap() instead of "sf3" because the 2010 census had no SF3.
      sumfile <- list("sf1", rlang::zap())
      
    } else {
      
      # To be fed to the .call argument of rlang::call_modify()
      .call <- alist(tidycensus::get_decennial())
      variables <- 
        sociome::decennial_vars %>%
        dplyr::filter(.data$year == !!year) %>% 
        eval(quote(split(variable, sumfile)), .)
      sumfile <- c("sf1", "sf3")
      
    }
    
    # i.e., if ACS data requested instead of decennial
  } else {
    
    # To be fed to the .call argument of rlang::call_modify()
    .call <- alist(tidycensus::get_acs())
    
    # Choosing the correct ACS variables is delegated out to its own function
    # since it's rather convoluted.
    variables <- list(choose_acs_variables(year, dataset, geography))
    
    survey <- dataset
    
    # There is no sumfile argument in tidycensus::get_acs()
    sumfile <- list(rlang::zap())
    
  }
  
  # To be fed to the MoreArgs argument in mapply()
  MoreArgs <-
    list(
      geography = geography,
      table = NULL,
      year = year,
      output = "tidy",
      keep_geo_vars = FALSE,
      summary_var = NULL,
      endyear = rlang::zap(),
      .homonyms = "first",
      .standardise = NULL,
      ...
    )
  
  if (!rlang::is_named(MoreArgs)) {
    stop("\nAdditional arguments passed to ... must all be named")
  }
  
  # mapply() was chosen because it conveniently will create as many list
  # elements as the longest element in ... (viz. in this case, the longest
  # element among .call, variables, sumfile, and survey), recycling all shorter
  # elements until they meet the length of the longest.
  mapply(
    FUN = rlang::call_modify,
    .call = .call,
    variables = variables,
    sumfile = sumfile,
    survey = survey,
    MoreArgs = MoreArgs,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
}



#' @importFrom rlang .data
get_tidycensus <- function(tidycensus_calls,
                           state_county, 
                           geography, 
                           year, 
                           dataset) {

  # There is special handling of this combination of arguments because it
  # requires tidycensus::get_decennial() to be called once for every state in
  # the reference area and tidycensus::get_acs() to be called once for every
  # county in the reference area. All other combinations of
  # geography/year/dataset either only call one of the two tidycensus functions
  # or require the same number of calls from each of them.
  if (geography == "tract" && year == 2010 && dataset == "decennial") {
    
    # state_county contains one row for each county already, so a call to
    # tidycensus::get_acs() (whose skeleton is tidycensus_calls[[2L]]) will be
    # created for every county in the reference area.
    acs_calls <-
      state_county %>%
      purrr::pmap(rlang::call_modify, .call = tidycensus_calls[[2L]])
    
    # The unique states within state_county are first extracted via
    # dplyr::distinct(), and then a call to tidycensus::get_decennial() (whose
    # skeleton is tidycensus_calls[[1L]]) will be created for each state in the
    # reference area.
    decennial_calls <-
      state_county %>% 
      dplyr::distinct(.data$state) %>% 
      purrr::pmap(rlang::call_modify, .call = tidycensus_calls[[1L]])
      
    
    message(
      "\n",
      length(acs_calls) + length(decennial_calls),
      " call(s) to tidycensus beginning."
    )
    
    # dplyr::select_if() pulls all non-geometry data columns to the left because
    # the geometry column is not an atomic vector (it's a list). This allows us
    # to select the inconsistently named Census variable name and Census
    # variable value columns by position.
    
    acs_data <- 
      acs_calls %>% 
      lapply(eval) %>% 
      do.call(rbind, .) %>% 
      dplyr::select_if(is.atomic) %>% 
      dplyr::select("GEOID", "NAME", names = 3L, values = 4L)
    
    data <-
      decennial_calls %>% 
      lapply(eval) %>% 
      do.call(rbind, .) %>% 
      dplyr::select_if(is.atomic) %>% 
      dplyr::select("GEOID", "NAME", names = 3L, values = 4L) %>% 
      dplyr::semi_join(as.data.frame(acs_data), by = "GEOID") %>% 
      rbind(acs_data)
    
    # Since get_decennial() calls are only broken up by state (see above), lots
    # of extra counties' data may be present. Since get_acs() is broken up by
    # county, it does not have this problem. Therefore, the results of the
    # former are filtered to only include tracts present in the results of the
    # latter.
    
  } else {
    message("\n", length(tidycensus_calls), " call(s) to tidycensus beginning.")
    
    # When we don't have to worry about the headache of different numbers of
    # calls needed for get_decennial() and get_acs(), we can simply use
    # tidyr::expand_grid() to create a separate call for each combination of the
    # elements of state_county and the elements of tidycensus_calls.
    data <-
      state_county %>% 
      tidyr::expand_grid(.call = tidycensus_calls) %>% 
      purrr::pmap(eval_tidycensus_call) %>% 
      do.call(rbind, .)
    
    # dplyr::select_if() pulls all non-geometry data columns to the left because
    # the geometry column is not an atomic vector (it's a list). This allows us
    # to select the inconsistently named Census variable name and Census
    # variable value columns by position.
    
  }
    
  # Since the contents of "data" may be the results of multiple different calls
  # to tidycensus function(s), sometimes the same geographic area (i.e., same
  # GEOID) will have inconsistent NAME or geometry values. The code below
  # essentially standardizes each GEOID's NAME and geometry, using the first
  # NAME and geometry value for each GEOID (found by match()).
  geoid_match <- data$GEOID %>% match(., .)
  data$NAME <- data$NAME[geoid_match]
  if (inherits(data, "sf")) {
    data$geometry <- data$geometry[geoid_match]
  }
  
  # tidyr::pivot_wider() didn't initially support sf-tibbles so we're using
  # tidyr::spread()
  
  # tidyr::pivot_wider(result, names_from = "names", values_from = "values")
  
  tidyr::spread(data, key = "names", value = "values")
}




eval_tidycensus_call <- function(...) {
  rlang::call_modify(...) %>% 
    eval() %>% 
    dplyr::select_if(is.atomic) %>% 
    dplyr::select("GEOID", "NAME", names = 3L, values = 4L)
}



#' @importFrom rlang .data
choose_acs_variables <- function(year, dataset, geography) {

  # See ?sociome::acs_vars for more info.
    
  set <-
    if (year >= 2011) {
      if (any(2015:2016 == year) && geography == "block group") {
        "set2"
      } else if (year == 2011 && dataset == "acs5") {
        "set3"
      } else {
        "set1"
      }
    } else if (year == 2010) {
      if (dataset == "acs5") {
        "set5"
      } else {
        "set4"
      }
    } else if (dataset == "acs1" && year >= 2008) {
      "set6"
    } else {
      "set7"
    }
  
  sociome::acs_vars$variable[sociome::acs_vars[[set]]]
}



exec_tidycensus <- function(state = NULL, county = NULL, ...) {
  if (!is.null(state)) {
    message("\nState: ", paste(state, collapse = ", "))
    if (!is.null(county)) {
      message("\nCounty: ", paste(county, collapse = ", "))
    }
  }
  exec_insistently(..., state = state, county = county)
}



exec_insistently <- 
  purrr::insistently(rlang::exec, rate = purrr::rate_delay(), quiet = FALSE)



filter_ref_area <- function(data, what, pattern, geo_length = NULL) {
  
  # Pattern is the list of GEOIDs in the ref_area object (in the function
  # environment of get_adi()). It is called "pattern" in the sense of regular
  # expressions: each element is ultimately turned into a regular expression.
  
  # First, each element in "pattern" is truncated as needed to the number of
  # characters invoked by the "geography" argument in get_adi() (e.g., 11
  # characters if geography = "tract"). This is necessary because users are
  # permitted (with a warning) to request ADI and ADI-3 at a level of geography
  # larger than any GEOID entered into the "geoid" argument (e.g.,
  # get_adi(geography = "county", geoid = c("31415926535", "271828182845905")))
  pattern_sub <-
    if (is.null(geo_length)) {
      pattern
    } else {
      stringr::str_sub(pattern, 1L, geo_length)
    }
  
  # Second, each GEOID pattern is prepended with "^" and matched to each GEOID
  # in "data"
  matches <-
    lapply(paste0("^", pattern_sub), stringr::str_which, string = data$GEOID)
  
  # User gets a warning if any GEOID pattern did not match any of the GEOIDs in
  # the tidycensus results.
  nomatch <- lapply(matches, length) == 0L
  if (any(nomatch)) {
    warning(
      "\nThe following ", what, "s had no match in census data:\n",
      paste(pattern[nomatch], collapse = ",\n")
    )
  }
  
  matches <- unique(unlist(matches, use.names = FALSE))
  # Returns any result in "data" that matched any GEOID pattern.
  data[matches, ]
}
