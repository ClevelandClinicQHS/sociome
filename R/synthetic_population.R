
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
                  quote(setNames(object = variable, nm = description)),
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
            setNames(
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
      GEOID = .data$GEOID,
      NAME = .data$NAME,
      total = .data$total,
      dplyr::across(
        .cols = 
          c(dplyr::contains("males_"),
            .data$hispanic_or_latino,
            .data$not_hispanic_or_latino_white_alone,
            .data$not_hispanic_or_latino_black_or_african_american_alone,
            .data$not_hispanic_or_latino_asian_alone,
            .data$not_hispanic_or_latino_american_indian_and_alaska_native_alone
          ),
        ~.x / total
      ),
      not_hispanic_or_latino_some_other_race_alone_or_two_or_more_races =
        1 -
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
  
  out
}


synthetic_population_column <- function(colname, total, ...) {
  dots <- c(...)
  dplyr::tibble(
    !!colname := sample(names(dots), size = total, replace = TRUE, prob = dots)
  )
}
