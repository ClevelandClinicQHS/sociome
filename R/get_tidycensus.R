
tidycensus_call <- function(.fn, ...) {
  args <- rlang::dots_list(..., .homonyms = "error")
  if (!rlang::is_named(args)) {
    stop("\nArguments passed to ... must all be named", call. = FALSE)
  }
  rlang::call2(.fn = .fn, !!!args, .ns = "tidycensus")
}



make_partial_tidycensus_calls <- function(dataset = c("decennial", "acs5",
                                                      "acs1", "acs3"),
                                          year = 2010,
                                          geography = "state",
                                          cache_tables = TRUE,
                                          geometry = TRUE,
                                          key = NULL,
                                          ...) {
  dataset <- match.arg(dataset)
  switch(
    dataset,
    decennial =

      # The 2010 decennial census did not gather the same detailed data that
      # the 2000 and 1990 censuses did (i.e., the 2010 census has no SF3), so
      # its gaps are filled with the 2010 5-year ACS estimates (in reality,
      # only a few data points are available from the 2010 decennial census
      # and most of the data is taken from the ACS)
      if (year == 2010) {
        get_decennial_vars <-
          sociome::decennial_vars[
            sociome::decennial_vars$year == 2010,
            "variable",
            drop = TRUE
          ]
        get_acs_vars <-
          sociome::acs_vars[sociome::acs_vars$dec2010, "variable", drop = TRUE]

        list(
          get_decennial =
            tidycensus_call(
              .fn = "get_decennial",
              geography = geography,
              variables = get_decennial_vars,
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
              variables = get_acs_vars,
              table = NULL,
              cache_table = cache_tables,
              year = 2010,
              output = "tidy",
              geometry = geometry,
              keep_geo_vars = FALSE,
              summary_var = NULL,
              key = key,
              survey = "acs5",
              ...
            )
        )

      } else {

        decennial_vars <-
          sociome::decennial_vars[sociome::decennial_vars$year == year, ]

        vars_by_sumfile <-
          split(decennial_vars$variable, f = decennial_vars$sumfile)

        vars_by_sumfile_tbl <-
          tibble::enframe(
            vars_by_sumfile,
            name = "sumfile",
            value = "variables"
          )

        purrr::pmap(
          vars_by_sumfile_tbl,
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

      acs_variables <-
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

      acs_variables <-
        sociome::acs_vars[
          sociome::acs_vars[[acs_variables]],
          "variable",
          drop = TRUE
        ]

      list(
        get_acs =
          tidycensus_call(
            .fn = "get_acs",
            geography = geography,
            variables = acs_variables,
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
    }
  )
}



#' @importFrom rlang .data
get_tidycensus <- function(geography,
                           state,
                           county,
                           geoid,
                           zcta,
                           year,
                           dataset,
                           partial_tidycensus_calls,
                           geometry,
                           evaluator) {

  # There are different location validation schemes for the three different user
  # input options concerning geography:

  # 1 ZCTA - use only zcta argument, leaving state, county, and geoid blank

  # 2 geoid - use only geoid argument, leaving state, county, and zcta blank

  # 3 state & county - use state and (optionally) county, leaving geoid and
  # zcta blank

  ref_area <-
    ref_area(
      geography = geography,
      state = state,
      county = county,
      zcta = zcta,
      geoid = geoid,
      year = year,
      dataset = dataset,
      partial_tidycensus_calls = partial_tidycensus_calls,
      evaluator = evaluator
    )

  tidycensus_data <-
    eval_tidycensus_calls(
      partial_tidycensus_calls = partial_tidycensus_calls,
      geography = geography,
      year = year,
      dataset = dataset,
      state_county = ref_area$state_county,
      geometry = geometry,
      evaluator = evaluator
    )

  # Since the call (or calls) to tidycensus functions usually gathers data on
  # more places than what the user specified, this pares the tidycensus-produced
  # data down to only include the user-specified reference area.
  if (!is.null(ref_area$geoid)) {
    tidycensus_data <-
      filter_ref_area(
        d = tidycensus_data,
        what = "GEOID",
        pattern = ref_area$geoid,
        geo_length = ref_area$geo_length
      )
  } else if (!is.null(ref_area$zcta)) {
    tidycensus_data <-
      filter_ref_area(
        d = tidycensus_data,
        what = "ZCTA",
        pattern = ref_area$zcta
      )
  }

  tidycensus_data
}


#' @importFrom rlang .data
eval_tidycensus_calls <- function(partial_tidycensus_calls,
                                  geography,
                                  year,
                                  dataset,
                                  state_county,
                                  geometry,
                                  evaluator) {
  if (geometry) {
    # Saves old tigris_use_cache value and puts it back when function exits
    old <- options(tigris_use_cache = TRUE)
    on.exit(options(old), add = TRUE)
  }

  # There is special handling of this combination of arguments because it
  # requires tidycensus::get_decennial() to be called once for every state in
  # the reference area and tidycensus::get_acs() to be called once for every
  # county in the reference area. All other combinations of
  # geography/year/dataset either only call one of the two tidycensus functions
  # or require the same number of calls from each of them.
  if (geography == "tract" && year == 2010 && dataset == "decennial" &&
      setequal(
        names(partial_tidycensus_calls),
        c("get_decennial", "get_acs")
      )
  ) {

    # state_county contains one row for each county already, so a call to
    # tidycensus::get_acs() will be created for every county in the reference
    # area.
    acs_calls <-
      purrr::pmap(
        .l = state_county,
        .f = rlang::call_modify,
        .call = partial_tidycensus_calls$get_acs
      )

    # The unique states within state_county are first extracted via
    # dplyr::distinct(), and then a call to tidycensus::get_decennial() will be
    # created for each state in the reference area.
    decennial_calls <-
      state_county %>%
      dplyr::distinct(.data$state) %>%
      purrr::pmap(
        rlang::call_modify,
        .call = partial_tidycensus_calls$get_decennial
      )

    message(
      "\n",
      length(acs_calls) + length(decennial_calls),
      " call(s) to tidycensus beginning."
    )

    acs_data <- lapply(acs_calls, evaluator)
    acs_data <- do.call(rbind, acs_data)
    acs_data <- wrangle_raw_tidycensus(acs_data, partial_tidycensus_calls)

    decennial_data <- lapply(decennial_calls, evaluator)
    decennial_data <- do.call(rbind, decennial_data)
    decennial_data <-
      wrangle_raw_tidycensus(decennial_data, partial_tidycensus_calls)

    # Since get_decennial() calls are only broken up by state (see above), lots
    # of extra counties' data may be present. Since get_acs() is broken up by
    # county, it does not have this problem. Therefore, the results of the
    # former are filtered to only include tracts present in the results of the
    # latter.
    decennial_data <-
      dplyr::semi_join(decennial_data, as.data.frame(acs_data), by = "GEOID")

    d <- rbind(decennial_data, acs_data)

  } else {

    # When we don't have to worry about the headache of different numbers of
    # calls needed for get_decennial() and get_acs(), we can simply use
    # tidyr::expand_grid() to create a separate call for each combination of the
    # elements of state_county and the elements of tidycensus_calls.
    tidycensus_calls <-
      state_county %>%
      tidyr::expand_grid(.call = partial_tidycensus_calls) %>%
      purrr::pmap(rlang::call_modify)

    message("\n", length(tidycensus_calls), " call(s) to tidycensus beginning.")

    d <- lapply(tidycensus_calls, evaluator)
    d <- lapply(d, wrangle_raw_tidycensus, partial_tidycensus_calls)
    d <- do.call(rbind, d)

  }

  # Since the contents of "d" may be the results of multiple different calls
  # to tidycensus function(s), sometimes the same geographic area (i.e., same
  # GEOID) will have inconsistent NAME or geometry values. The code below
  # essentially standardizes each GEOID's NAME and geometry, using the first
  # NAME and geometry value for each GEOID (found by match()).
  geoid_match <- d$GEOID %>% match(., .)
  d$NAME <- d$NAME[geoid_match]
  if (inherits(d, "sf")) {
    d$geometry <- d$geometry[geoid_match]
  }

  if (all(c("names_to_spread", "values_to_spread") %in% names(d))) {
    # tidyr::pivot_wider() didn't initially support sf-tibbles so we didn't
    # end up implementing it. tidyr::pivot_wider(result, names_from = "names",
    # values_from = "values")
    # cols_in_order <- unique(d$names_to_spread)
    d <- tidyr::spread(d, key = "names_to_spread", value = "values_to_spread")
    # d <- d[c("GEOID", "NAME", cols_in_order)]
  }

  d
}



wrangle_raw_tidycensus <- function(d, partial_tidycensus_calls) {
  # dplyr::select_if() pulls all non-geometry columns to the left because the
  # geometry column is not an atomic vector (it's a list). This allows us to
  # select the inconsistently named Census variable name and Census variable
  # value columns by position.
  d <- dplyr::select_if(d, is.atomic)

  # We then wrangle the data depending on whether the tidycensus called
  # requested "tidy" or "wide" data.
  switch(
    partial_tidycensus_calls[[1L]]$output,
    tidy = {
      d <-
        dplyr::select(
          d,
          "GEOID",
          "NAME",
          names_to_spread = 3L,
          values_to_spread = 4L
        )
    },
    wide = {
      d <- dplyr::select(d, "GEOID", "NAME", 3L)
    }
  )

  d
}



filter_ref_area <- function(d, what, pattern, geo_length = NULL) {

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

  geoid <-
    switch(what, ZCTA = stringr::str_extract(d$GEOID, "\\d{5}$"), d$GEOID)

  # Second, each GEOID pattern is prepended with "^" and matched to each GEOID
  # in "d"
  matches <-
    lapply(paste0("^", pattern_sub), stringr::str_which, string = geoid)

  # User gets a warning if any GEOID pattern did not match any of the GEOIDs in
  # the tidycensus results.
  nomatch <- lengths(matches, use.names = FALSE) == 0L
  if (any(nomatch)) {
    warning(
      "\nThe following ", what, "s had no match in census data:\n",
      paste(pattern[nomatch], collapse = ",\n")
    )
  }

  matches <- unique(unlist(matches, use.names = FALSE))
  # Returns any result in "d" that matched any GEOID pattern.
  d[matches, , drop = FALSE]
}
