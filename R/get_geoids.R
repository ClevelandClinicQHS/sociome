#' Obtain GEOIDs of areas
#'
#' Returns a [`tibble`][tibble::tibble] or [`sf`][sf::sf] of GEOIDs, names, and
#' decennial census population of user-specified locations.
#'
#' This allows users to quickly obtain all GEOIDs in a specified location at a
#' specific level of geography without having to manually look them up somewhere
#' else.
#'
#' This facilitates calls to [get_adi()] that involve somewhat complicated
#' reference areas.
#'
#' @param geography A character string denoting the level of census geography
#'   whose GEOIDs you'd like to obtain. Must be one of `c("state", "county",
#'   "tract", "block group", "block", "zcta")` (`"zip code tabulation area"`
#'   will be automatically changed to `"zcta"`).
#' @param year Single integer specifying the year of US Census data to use.
#' @param ... Additional arguments to be passed to
#'   [tidycensus::get_decennial()]. Use at your own risk.
#' @inheritParams get_adi
#'
#' @seealso [`dataset_year_geography_availability`] for usable combinations of
#'   `dataset`, `year`, and `geography`.
#' @examples
#' \dontrun{
#' # Wrapped in \dontrun{} because it requires a Census API key.
#'
#' # Get all tract GEOIDs for Manhattan
#' tracts <- get_geoids(geography = "tract", state = "New York", county = "New York")
#' tracts
#'
#' # Get all block GEOIDs for the fifth tract on that list
#' get_geoids(geography = "block", geoid = tracts$GEOID[5])
#' }
#' @export
get_geoids <- function(geography = c("state", "county", "tract", "block group",
                                     "block", "zcta",
                                     "zip code tabulation area"),
                       state = NULL,
                       county = NULL,
                       geoid = NULL,
                       zcta = NULL,
                       year = 2020,
                       dataset = c("acs5", "decennial", "acs3", "acs1"),
                       geometry = FALSE,
                       cache_tables = TRUE,
                       key = NULL,
                       evaluator =
                         purrr::insistently(
                           eval,
                           rate = purrr::rate_delay(),
                           quiet = FALSE
                          ),
                       ...) {
  geography <- validate_geography(match.arg(geography))
  year <- validate_year(year)
  dataset <-
    validate_dataset(match.arg(dataset), year, geography, type = "population")

  # Create the call skeleton to get_decennial() using the validated argument
  # list
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
                stats::setNames(
                  object =
                    if (year == 2020) "P1_001N" else
                      if (year == 1990) "P0010001" else "P001001",
                  nm = paste0("census_", year, "_pop")
                ),
              table = NULL,
              cache_table = cache_tables,
              year = year,
              sumfile = if (year == 2010) "sf1" else if (year == 2020) "dhc",
              geometry = geometry,
              output = "wide",
              key = key,
              ...
            )
        ),
      list(
        get_acs =
          tidycensus_call(
            .fn = "get_acs",
            geography = geography,
            variables =
              stats::setNames(
                "B01001_001",
                paste(dataset, year, "pop", sep = "_")
              ),
            table = NULL,
            cache_table = cache_tables,
            year = year,
            output = "wide",
            geometry = geometry,
            key = key,
            survey = dataset,
            ...
          )
      )
    )

  d <-
    get_tidycensus(
      geography = geography,
      state = state,
      county = county,
      geoid = geoid,
      zcta = zcta,
      year = year,
      dataset = dataset,
      partial_tidycensus_calls = partial_tidycensus_calls,
      geometry = geometry,
      evaluator = evaluator
    )

  names(d) <- stringr::str_replace(names(d), "_popE$", "_pop")

  d
}
