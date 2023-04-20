

#' Create a synthetic population simulating US Census areas
#'
#' Returns a data set of synthetic individuals based on user-specified US Census
#' areas. The age, sex, race, and ethnicity of each individual is probabilistic,
#' based on the demographics of the areas as reported in a user-specified US
#' Census data set.
#'
#' Returns a [`tibble`][tibble::tibble] or [`sf`][sf::sf] object where each row
#' represents a synthetic person. Each person has an age, sex, race, and
#' ethnicity. The probability of what each person's age/sex/race/ethnicity will
#' be is equal to the proportions in their census area as reported in the
#' user-specified US Census data set (e.g., 2010 Decennial Census or 2017 ACS
#' 5-year estimates). The number of rows in the data set will equal the number
#' of people living in the user-specified US Census areas, as reported in the
#' same US Census data set.
#'
#' @param geography A character string denoting the level of US census geography
#'   at which you want to create a synthetic population. Required.
#' @param state A character string specifying states whose population you want
#'   to synthesize. Defaults to `NULL`. Can contain full state names, two-letter
#'   state abbreviations, or a two-digit FIPS code/GEOID (must be a vector of
#'   strings, so use quotation marks and leading zeros if necessary). Must be
#'   left as `NULL` if using the `geoid` or `zcta` parameter.
#' @param county A vector of character strings specifying the counties whose
#'   population you want to synthesize. Defaults to `NULL`. If not `NULL`, the
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
#' @param year,dataset Specifies the US Census data set on which to base the
#'   demographic profile of your synthetic population.
#'
#'   `year` must be a single integer specifying the year of US Census data to
#'   use.The data set used to calculate ADIs and ADI-3s.
#'
#'   `dataset` must be one of `c("acs5", "acs3", "acs1", "decennial")`, denoting
#'   the 5-, 3-, and 1-year ACS along with the decennial census. Defaults to
#'   `"acs5"`.
#'
#'   When `dataset = "decennial"`, `year` must be in `c(1990, 2000, 2010)`.
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
#' @param cache_tables The plural version of the `cache_table` argument in
#'   [tidycensus::get_acs()] or [tidycensus::get_decennial()]. (`get_adi()`
#'   calls the necessary `tidycensus` function many times in order to return
#'   ADIs and ADI-3s, so many tables are cached if `TRUE`). Defaults to `TRUE`.
#' @param max_age A single integer representing the largest possible age that
#'   can appear in the data set. Simulated age values exceeding this value will
#'   be top-coded to this value. Defaults to 115. See details.
#' @param rate A single number, passed to [stats::rexp()] when synthesizing the
#'   ages of the highest age bracket. Defaults to 0.25. See details.
#' @param key Your Census API key as a character string. Obtain one at
#'   <http://api.census.gov/data/key_signup.html>. Defaults to `NULL`. Not
#'   necessary if you have already loaded your key with [census_api_key()].
#' @param seed Passed onto [set.seed()], which is called before
#'   probabilistically synthesizing the age values with [sample()].
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
#' @section Synthesizing ages from US Census Data: US Census data provides
#'   counts of the number of people in different age brackets of varying widths.
#'   The `age_lo` and `age_hi` columns in the output depict the age bracket of
#'   each individual in the synthetic population. There is also an `age` column
#'   that probabilistically generates a non-whole-number age within the age
#'   bracket. A uniform distribution (via [`stats::runif()`]) guides this age
#'   generation for all age brackets except the highest age bracket ("age 85 and
#'   over" in the extant ACS and Decennial Census data). An exponential
#'   distribution (via [`stats::rexp()`]) guides the age generation for this
#'   highest age bracket, and the user can specify `rate` to customize the
#'   exponential distribution that is used.
#'   
#' @return If `geometry = FALSE`, (the default) a [`tibble`][tibble::tibble]. If
#'   `geometry = TRUE` is specified, an [`sf`][sf::sf].
#' @examples
#' # Synthetic population for Utah, using the 2019 ACS 5-year estimates:
#' synthetic_population(geography = "state", state = "UT", year = 2019)
#' 
#' # Same, but make it so that survival past age 85 is highly unlikely
#' # (via rate = 10), and so that 87 is the maximum possible age
#' synthetic_population(
#'   geography = "state",
#'   state = "UT",
#'   year = 2019,
#'   max_age = 87,
#'   rate = 10
#' )
#' 
#' # Synthetic population of the Delmarva Peninsula at the census tract level,
#' # using 2000 Decennial Census data
#' synthetic_population(
#'   geography = "tract",
#'   geoid = 
#'     # This two-digit GEOID is the state of Delaware.
#'     c("10",
#'     
#'     # These five-digit GEOIDs are specific counties in Virginia and Maryland
#'       "51001", "51131", "24015", "24029", "24035", "24011", "24041", "24019",
#'       "24045", "24039", "24047"),
#'   year = 2000,
#'   dataset = "decennial"
#' )
#' @importFrom rlang .data
#' @export
synthetic_population <- function(geography,
                                 state           = NULL,
                                 county          = NULL,
                                 geoid           = NULL,
                                 zcta            = NULL,
                                 year,
                                 dataset         = c("acs5", "acs3", "acs1",
                                                     "decennial"),
                                 geometry        = FALSE,
                                 cache_tables    = TRUE,
                                 max_age         = 115,
                                 rate            = 0.25,
                                 key             = NULL,
                                 seed            = NULL,
                                 ...) {
  geography <- validate_geography(geography)
  year      <- validate_year(year)
  dataset   <- validate_dataset(dataset, year, geography)
  
  partial_tidycensus_calls <-
    switch(
      dataset, 
      decennial =
        list(
          get_decennial = 
            tidycensus_call(
              .fn = "get_decennial",
              geography = geography,
              variables =
                eval(
                  quote(stats::setNames(object = variable, nm = description)),
                  dplyr::filter(
                    sociome::decennial_age_sex_race_ethnicity_vars,
                    .data$year == !!year
                  )
                ),
              table = NULL,
              cache_table = cache_tables,
              year = year,
              sumfile = "sf1",
              geometry = geometry,
              output = "tidy",
              keep_geo_vars = FALSE,
              summary_var = NULL,
              key = key,
              ...
            )
        ),
      list(
        tidycensus_call(
          .fn = "get_acs",
          geography = geography,
          variables =
            stats::setNames(
              object = sociome::acs_age_sex_race_ethnicity_vars$variable,
              nm = sociome::acs_age_sex_race_ethnicity_vars$description
            ),
          table = NULL,
          cache_table = cache_tables,
          year = year,
          output = "tidy",
          geometry = geometry,
          keep_geo_vars = FALSE,
          summary_var = NULL,
          key = key,
          survey = dataset,
          ...
        )
      )
    )
  
  raw_census_data <-
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
  
  census_data_prepped <-
    dplyr::transmute(
      raw_census_data,
      dplyr::across(
        c(.data$GEOID,
          .data$NAME,
          .data$total,
          dplyr::contains("males_"),
          .data$hispanic_or_latino,
          .data$not_hispanic_or_latino_white_alone,
          .data$not_hispanic_or_latino_black_or_african_american_alone,
          .data$not_hispanic_or_latino_asian_alone,
          .data$not_hispanic_or_latino_american_indian_and_alaska_native_alone
        )
      ),
      not_hispanic_or_latino_some_other_race_alone_or_two_or_more_races =
        .data$total - 
        .data$hispanic_or_latino -
        .data$not_hispanic_or_latino_white_alone -
        .data$not_hispanic_or_latino_black_or_african_american_alone -
        .data$not_hispanic_or_latino_asian_alone -
        .data$not_hispanic_or_latino_american_indian_and_alaska_native_alone
    )
  
  out <-
    dplyr::transmute(
      census_data_prepped,
      GEOID = .data$GEOID,
      NAME = .data$NAME,
      age_sex = 
        purrr::pmap(
          dplyr::across(c(.data$total, dplyr::contains("males_"))),
          synthetic_population_column,
          colname = "age_sex"
        ),
      race_ethnicity =
        purrr::pmap(
          dplyr::across(c(.data$total, dplyr::contains("hispanic_or_latino"))),
          synthetic_population_column,
          colname = "race_ethnicity"
        )
    )
  
  out <- tidyr::unnest(out, cols = c(.data$age_sex, .data$race_ethnicity))
  
  ages <- stringr::str_extract_all(out$age_sex, "\\d+", simplify = TRUE)
  storage.mode(ages) <- "double"
  
  over <- stringr::str_detect(out$age_sex, "over")
  
  set.seed(seed)
  
  out <-
    dplyr::transmute(
      out,
      .data$GEOID,
      .data$NAME,
      .data$age_sex,
      sex = stringr::str_extract(.data$age_sex, "^(fe)?male"),
      age_lo = 
        ifelse(
          test = stringr::str_detect(.data$age_sex, "under"),
          yes = 0,
          no = 
            !!ages[, 1L] +
            stringr::str_detect(
              .data$age_sex,
              "(more[^[:alnum:]]?than|over|>(?!=))[^[:alnum:]]?\\d"
            )
        ),
      age_hi =
        dplyr::coalesce(
          !!ages[, 2L],
          ifelse(
            test = !!over,
            yes = !!max_age,
            no = 
              !!ages[, 1L] -
              stringr::str_detect(
                .data$age_sex,
                "(less[^[:alnum:]]?than|under|<(?!=))[^[:alnum:]]?\\d"
              )
          )
        ),
      age =
        ifelse(
          !!over,
          pmin(
            .data$age_lo + stats::rexp(n = dplyr::n(), rate = !!rate), !!max_age
          ),
          stats::runif(dplyr::n(), min = .data$age_lo, max = .data$age_hi + 1)
        ),
      .data$race_ethnicity
    )
  
  out
}


synthetic_population_column <- function(colname, total, ...) {
  if (is.na(total) || total == 0) {
    return(dplyr::tibble(!!colname := character()))
  }
  
  dots <- c(...)
  
  dots[is.na(dots)] <- 0
  
  dplyr::tibble(
    !!colname := sample(names(dots), size = total, replace = TRUE, prob = dots)
  )
}

