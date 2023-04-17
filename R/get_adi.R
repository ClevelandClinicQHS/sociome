#' Get Area Deprivation Index (ADI) and Berg Indices (ADI-3)
#'
#' Returns the ADI and ADI-3 of user-specified areas.
#'
#' Returns a [`tibble`][tibble::tibble] or [`sf`][sf::sf] object of the Area
#' Deprivation Indices (ADIs) and Berg Indices (ADI-3s) of user-specified
#' locations in the United States, utilizing US Census data. Locations that are
#' listed as having zero households are excluded from ADI and ADI-3 calculation:
#' their ADI and ADI-3 values will be `NA`.
#'
#' @param geography A character string denoting the level of census geography
#'   whose ADIs and ADI-3s you'd like to obtain. Must be one of `c("state",
#'   "county", "tract", "block group", "zcta")`. Required.
#' @param state A character string specifying states whose ADI and ADI-3 data is
#'   desired. Defaults to `NULL`. Can contain full state names, two-letter state
#'   abbreviations, or a two-digit FIPS code/GEOID (must be a vector of strings,
#'   so use quotation marks and leading zeros if necessary). Must be left as
#'   `NULL` if using the `geoid` or `zcta` parameter.
#' @param county A vector of character strings specifying the counties whose ADI
#'   and ADI-3 data you're requesting. Defaults to `NULL`. If not `NULL`, the
#'   `state` parameter must have a length of 1. County names and three-digit
#'   FIPS codes are accepted (must contain strings, so use quotation marks and
#'   leading zeros if necessary). Must be blank if using the `geoid` parameter.
#' @param geoid A character vector of GEOIDs (use quotation marks and leading
#'   zeros). Defaults to `NULL`. Must be blank if `state`, `county`, or `zcta`
#'   is used. Can contain different levels of geography (see details).
#' @param zcta A character vector of ZCTAs or the leading digit(s) of ZCTAs (use
#'   quotation marks and leading zeros). Defaults to `NULL`. Must be blank if
#'   `state`, `county`, or `geoid` is used.
#'
#'   Strings under 5 digits long will yield all ZCTAs that begin with those
#'   digits.
#'
#'   Requires that `geography = "zcta"`. If `geography = "zcta"` and `zcta =
#'   NULL`, all ZCTAs in the US will be used.
#' @param year Single integer specifying the year of US Census data to use.
#' @param dataset The data set used to calculate ADIs and ADI-3s. Must be one of
#'   `c("acs5", "acs3", "acs1", "decennial")`, denoting the 5-, 3-, and 1-year
#'   ACS along with the decennial census. Defaults to `"acs5"`.
#'
#'   When `dataset = "decennial"`, `year` must be in `c(1990, 2000, 2010)`.
#'
#'   The 2010 decennial census did not include the long-form questionnaire used
#'   in the 1990 and 2000 censuses, so this function uses the 5-year estimates
#'   from the 2010 ACS to supply the data not included in the 2010 decennial
#'   census. In fact, the only 2010 decennial variables used are H003002,
#'   H014002, P020002, and P020008.
#'
#'   Important: data are not always available depending on the level of
#'   geography and data set chosen. See
#'   <https://www.census.gov/programs-surveys/acs/guidance/estimates.html>.
#' @param geometry Logical value indicating whether or not shapefile data should
#'   be included in the result, making the result an [`sf`][sf::sf] object
#'   instead of a plain [`tibble`][tibble::tibble]. Defaults to `FALSE`.
#'
#'   The shapefile data that is returned is somewhat customizable by passing
#'   certain arguments along to the `tidycensus` functions via `...`.
#' @param keep_indicators Logical value indicating whether or not the resulting
#'   [`tibble`][tibble::tibble] or [`sf`][sf::sf] object will contain the
#'   socioeconomic measures used to calculate the ADI and ADI-3 values. Defaults
#'   to `FALSE`.
#'
#'   See [`acs_vars`] and [`decennial_vars`] for basic descriptions of the raw
#'   census variables.
#' @param cache_tables The plural version of the `cache_table` argument in
#'   [tidycensus::get_acs()] or [tidycensus::get_decennial()]. (`get_adi()`
#'   calls the necessary `tidycensus` function many times in order to return
#'   ADIs and ADI-3s, so many tables are cached if `TRUE`). Defaults to `TRUE`.
#' @param key Your Census API key as a character string. Obtain one at
#'   <http://api.census.gov/data/key_signup.html>. Defaults to `NULL`. Not
#'   necessary if you have already loaded your key with [census_api_key()].
#' @param raw_data_only Logical, indicating whether or not to skip calculation
#'   of the ADI and ADI-3 and only return the census variables. Defaults to
#'   `FALSE`.
#' @param seed Passed to [calculate_adi()].
#' @param ... Additional arguments to be passed onto [tidycensus::get_acs()] or
#'   [tidycensus::get_decennial()]. These must all be named. Must not match any
#'   of the `tidycensus` formal arguments that `sociome` needs to set
#'   explicitly.
#'
#'   This may be found to be helpful when setting `geometry = TRUE`, since the
#'   `tidycensus` functions pass `...` onto the appropriate `tigris` function
#'   (namely, one of [tigris::states()], [tigris::counties()],
#'   [tigris::tracts()], [tigris::block_groups()], or [tigris::zctas()],
#'   according to the the value of `geography`). This enables the user to
#'   somewhat customize the shapefile data obtained.
#'
#' @section Reference area: **The concept of "reference area" is important to
#'   understand when using this function.** The algorithm that produced the
#'   original ADIs employs factor analysis. As a result, the ADI is a relative
#'   measure; the ADI of a particular location is dynamic, varying depending on
#'   which other locations were supplied to the algorithm. In other words, **ADI
#'   will vary depending on the reference area you specify.**
#'
#'   For example, the ADI of Orange County, California is *x* when calculated
#'   alongside all other counties in California, but it is *y* when calculated
#'   alongside all counties in the US. The `get_adi()` function enables the user
#'   to define a **reference area** by feeding a vector of GEOIDs to its `geoid`
#'   parameter (or alternatively for convenience, states and/or counties to
#'   `state` and `county`). The function then gathers data from those specified
#'   locations and performs calculations using their data alone.
#'
#'   The Berg Indices (ADI-3) were developed with this principle of relativity
#'   in mind, and as such there is no set of seminal ADI-3 values. Thus, the
#'   terms "Berg Indices" and "ADI-3" refer more nearly to any values generated
#'   using the algorithm employed in this package.
#'
#'   Areas listed as having zero households are excluded from the reference
#'   area, and their ADI and ADI-3 values will be `NA`.
#'
#' @section The `geoid` parameter: Elements of `geoid` can represent different
#'   levels of geography, but they all must be either 2 digits (for states), 5
#'   digits (for counties), 11 digits (for tracts), or 12 digits (for block
#'   groups). It must contain character strings, so use quotation marks as well
#'   as leading zeros where applicable.
#'
#' @section ADI and ADI-3 factor loadings: The returned
#'   [`tibble`][tibble::tibble] or [`sf`][sf::sf] is of class `adi`, and it
#'   contains an attribute called `loadings`, which contains a tibble of the PCA
#'   loadings of each factor. This is accessible through
#'   [`attr`]`(name_of_tibble, "loadings")`.
#'
#' @section Missingness and imputation: While this function allows flexibility
#'   in specifying reference areas (see the **Reference area** section above),
#'   data from the US Census are masked for sparsely populated places, resulting
#'   in many missing values.
#'
#'   Imputation is attempted via [`mice::mice`]`(m = 1, maxit = 50, method =
#'   "pmm", seed = seed)`. If imputation is unsuccessful, an error is thrown,
#'   but the dataset of indicators on which imputation was unsuccessful is
#'   available via [rlang::last_error()]`$adi_indicators` and the raw census
#'   data are available via [rlang::last_error()]`$adi_raw_data`. The former
#'   excludes areas with zero households, but the latter includes them.
#'
#'   One of the indicators of both ADI and the Financial Strength component of
#'   ADI-3 is median family income, but methodological issues with the 2015 and
#'   2016 ACS have rendered this variable unavailable at the block group level
#'   for those years. When requested, this function will use median household
#'   income in its place, with a `warning()`. See
#'   <https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2016-01.html>.
#'
#' @section API-related error handling: Depending on user input, this function
#'   may call its underlying functions ([tidycensus::get_acs()] or
#'   [tidycensus::get_decennial()]) many times in order to accommodate their
#'   behavior. When these calls are broken up by state or by state and county, a
#'   message is printed indicating the state or state and county whose data is
#'   being pulled. These calls are wrapped in
#'   [`purrr::insistently`]`(`[purrr::rate_delay()]`, quiet = FALSE)`, meaning
#'   that they are attempted over and over until success, and `tidycensus` error
#'   messages are printed as they occur.
#'
#' @section Warnings and disclaimers: Please note that this function calls data
#'   from US Census servers, so execution may take a long time depending on the
#'   user's internet connection and the amount of data requested.
#'
#'   For advanced users, if changing the `dataset` argument, be sure to know the
#'   advantages and limitations of the 1-year and 3-year ACS estimates. See
#'   <https://www.census.gov/programs-surveys/acs/guidance/estimates.html.> for
#'   details.
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
#' @return If `geometry = FALSE`, (the default) a [`tibble`][tibble::tibble]. If
#'   `geometry = TRUE` is specified, an [`sf`][sf::sf].
#' @importFrom rlang .data
#' @export
get_adi <- function(geography,
                    state           = NULL,
                    county          = NULL,
                    geoid           = NULL,
                    zcta            = NULL,
                    year,
                    dataset         = c("acs5", "acs3", "acs1", "decennial"),
                    geometry        = FALSE,
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
  # tidycensus::get_acs() and/or tidycensus::get_decennial(). The following
  # creates the skeletons of these calls, irrespective of location (i.e.,
  # state/county/geoid/zcta)
  partial_tidycensus_calls <-
    switch(
      dataset,
      decennial = 
        
        # The 2010 decennial census did not gather the same detailed data that
        # the 2000 and 1990 censuses did (i.e., the 2010 census has no SF3), so
        # its gaps are filled with the 2010 5-year ACS estimates (in reality,
        # only a few data points are available from the 2010 decennial census
        # and most of the data is taken from the ACS)
        if (year == 2010) {
          
          list(
            get_decennial =
              tidycensus_call(
                .fn = "get_decennial",
                geography = geography,
                variables = 
                  sociome::decennial_vars[
                    sociome::decennial_vars$year == 2010,
                    "variable",
                    drop = TRUE
                  ],
                table = NULL,
                cache_table = cache_tables,
                year = 2010,
                sumfile = "sf1",
                geometry = geometry,
                output = "tidy",
                keep_geo_vars = FALSE,
                summary_var = NULL,
                key = key,
                ...
              ),
            get_acs =
              tidycensus_call(
                .fn = "get_acs",
                geography = geography,
                variables =
                  sociome::acs_vars$variable[sociome::acs_vars$dec2010],
                table = NULL,
                cache_table = cache_tables,
                year = 2010,
                output = "tidy",
                geomety = geometry,
                keep_geo_vars = FALSE,
                summary_var = NULL,
                key = key,
                survey = "acs5",
                ...
              )
          )
          
        } else {
          
          # Everything prior to pmap() creates 2 by 2 tibble with column
          # sumfile = c("sf1", "sf3") and list column "variables" that
          # contains the corresponding SF1 and SF3 variables.
          sociome::decennial_vars[sociome::decennial_vars$year == year, ] %>% 
            eval(expr = quote(split(variable, f = sumfile))) %>% 
            tibble::enframe(name = "sumfile", value = "variables") %>% 
            purrr::pmap(
              .f = tidycensus_call,
              .fn = "get_decennial",
              geography = geography,
              table = NULL, 
              cache_table = cache_tables,
              year = year,
              geometry = geometry,
              output = "tidy",
              keep_geo_vars = FALSE,
              summary_var = NULL,
              key = key,
              ...
            )
        },
      
      # i.e., if ACS data requested instead of decennial
      {
        
        variables_set <- 
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
        
        list(
          get_acs =
            tidycensus_call(
              .fn = "get_acs",
              geography = geography,
              variables = 
                sociome::acs_vars[
                  sociome::acs_vars[[variables_set]],
                  "variable",
                  drop = TRUE
                ],
              table = NULL,
              cache_table = cache_tables,
              year = year,
              output = "tidy",
              geomety = geometry,
              keep_geo_vars = FALSE,
              summary_var = NULL,
              key = key,
              survey = dataset,
              ...
            )
        )
      }
    )
  
  raw_data <-
    get_tidycensus(
      geography = geography,
      state = state,
      county = county,
      geoid = geoid,
      zcta = zcta,
      year = year,
      dataset = dataset,
      partial_tidycensus_calls = partial_tidycensus_calls,
      geometry = geometry
    )
  
  if (raw_data_only) {
    raw_data
  } else {
    calculate_adi(raw_data, keep_indicators = keep_indicators, seed = seed)
  }
}


# 
# exec_tidycensus <- function(state = NULL, county = NULL, ...) {
#   if (!is.null(state)) {
#     message("\nState: ", paste(state, collapse = ", "))
#     if (!is.null(county)) {
#       message("\nCounty: ", paste(county, collapse = ", "))
#     }
#   }
#   exec_insistently(..., state = state, county = county)
# }


