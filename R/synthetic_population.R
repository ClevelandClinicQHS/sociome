
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
        total - 
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
          pmin(.data$age_lo + rexp(n = dplyr::n(), rate = !!rate), !!max_age),
          runif(dplyr::n(), min = .data$age_lo, max = .data$age_hi + 1)
        ),
      .data$race_ethnicity
    )
  
  out
}


synthetic_population_column <- function(colname, total, ...) {
  dots <- c(...)
  
  if (is.na(total) || total == 0) {
    return(dplyr::tibble(!!colname := character()))
  }
  
  dots[is.na(dots)] <- 0
  
  dplyr::tibble(
    !!colname := sample(names(dots), size = total, replace = TRUE, prob = dots)
  )
}

