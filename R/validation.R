
validate_geography <- function(geography = c("state",
                                             "county", 
                                             "tract",
                                             "block group",
                                             "zcta",
                                             "zip code tabulation area")) {
  geography <- match.arg(geography)
  
  if (geography == "zcta") {
    geography <- "zip code tabulation area"
  }
  
  geography
}



validate_single_positive_integer <- function(x, what) {
  if (!rlang::is_scalar_integerish(x, finite = TRUE) || x < 1L) {
    stop(what, " must be a single positive integer", call. = FALSE)
  }
  as.integer(x)
}




validate_year <- function(year, values = NULL) {
  year <- validate_single_positive_integer(year, "year")
  if (!is.null(values) && !any(values == year)) {
    stop("year must be one of:\n", paste0(values, collapse = ", "),
         call. = FALSE)
  }
  year
}



validate_dataset <- function(dataset = c("acs5", "acs3", "acs1", "decennial"),
                             year,
                             geography) {
  
  dataset <- match.arg(dataset)
  
  if (dataset == "decennial") {
    
    if (!any(c(1990, 2000, 2010) == year)) {
      stop(
        'When setting dataset = "decennial", year must be 1990, 2000, or 2010.',
        call. = FALSE
      )
    }
    
    if (geography == "zip code tabulation area") {
      stop(
        'geography = "zip code tabulation area" not supported',
        '\nfor the decennial census data.',
        '\nTry dataset = "acs5" (or "acs3" or "acs1")',
        call. = FALSE
      )
    }
    
  } else if (geography == "block group" && any(2015:2016 == year)) {
    warning(
      "\nMedian family income (B19113_001) is unavailable at the block group",
      "\nlevel in the years 2015 and 2016.",
      "\n\nMedian household income (B19013_001) will be used instead.",
      '\n\nSee the "Missingness and imputation" section of ?get_adi, ',
      "as well as:",
      "\nhttps://www.census.gov/programs-surveys/acs/",
      "technical-documentation/user-notes/2016-01.html",
      call. = FALSE,
      immediate. = TRUE
    )
  }
  
  dataset
}



determine_input_arg <- function(geoid = NULL, 
                                state = NULL,
                                county = NULL,
                                zcta = NULL) {
  args <- c("geoid"[!is.null(geoid)], "state"[!is.null(state)],
            "county"[!is.null(county)], "zcta"[!is.null(zcta)])
  
  switch(
    length(args) + 1L,
    return("NULL"),
    switch(
      args,
      county = stop("Can't enter county when state is NULL", call. = FALSE),
      return(args)
    ),
    if (setequal(args, c("state", "county"))) {
      return("county")
    } else if (setequal(args, c("longitude", "latitude"))) {
      return("lon_lat")
    }
  )
  
  stop("Enter only one of:", if (!missing(zcta)) "\nzcta,",
       "\ngeoid,\nor state/county",
       call. = FALSE)
}



validate_state <- function(state) {
  
  state <- validate_single_string(state, what = "state")
  
  i <-
    if (stringr::str_detect(state, "^\\d{2,}$")) {
      if (nchar(state) > 2L) {
        warning('More than two digits detected in state = "', state, '".',
                '\nUsing state = "', state <- substr(state, 1L, 2L),
                '" instead', call. = FALSE)
      }
      match(state, tidycensus::fips_codes$state_code)
    } else if (stringr::str_detect(state, "^[A-Za-z]{2}$")) {
      match(toupper(state), tidycensus::fips_codes$state)
    } else {
      match(toupper(state), toupper(tidycensus::fips_codes$state_name))
    }
  
  if (is.na(i)) {
    stop('state = "', state,
         '" does not match any state in tidycensus::fips_codes.',
         "\nReview that table and then review your input.", call. = FALSE)
  }
  
  tidycensus::fips_codes$state_code[i]
}






validate_county <- function(state, county) {
  state <- validate_state(state)
  validate_single_string(county, what = "county")
  
  if (stringr::str_detect(county, "^(\\d{2})?\\d{3}$")) {
    if (nchar(county) == 5L) {
      if (substr(county, 1L, 2L) != state) {
        stop('The first two digits of county = "', county,
             '" do not match\nthe GEOID of state = "', state, '"',
             call. = FALSE)
      }
      county <- substr(county, 3L, 5L)
    }
    i <- which(tidycensus::fips_codes$state_code == state &
                 tidycensus::fips_codes$county_code == county)
  } else {
    pattern <- paste0("(?i)^", county)
    i <-
      which(
        tidycensus::fips_codes$state_code == state &
          stringr::str_detect(
            string = tidycensus::fips_codes$county,
            pattern = paste0("(?i)^", county)
          )
      )
  }
  
  if (length(i) != 1L) {
    if (length(i)) {
      stop('county = "', county, '" matches more than one county in state = "',
           state, '":\n',
           paste(
             tidycensus::fips_codes$county_code[i],
             tidycensus::fips_codes$county[i],
             collapse = "\n"
           ),
           call. = FALSE)
    }
    stop('county = "', county, '" does not match any counties in state = "',
         state, '"', call. = FALSE)
  }
  
  paste0(
    tidycensus::fips_codes$state_code[i],
    tidycensus::fips_codes$county_code[i]
  )
}





get_ref_area <- function(geoid,
                         state, 
                         county, 
                         zcta, 
                         geography,
                         year,
                         dataset,
                         tidycensus_calls) {
  
  # There are different location validation schemes for the three different user
  # input options concerning geography:
  
  # 1 ZCTA - use only zcta argument, leaving state, county, and geoid blank
  
  # 2 geoid - use only geoid argument, leaving state, county, and zcta blank
  
  # 3 state & county - use state and (optionally) county, leaving geoid and
  # zcta blank
  
  switch(
    determine_input_arg(geoid = geoid, state = state, county = county,
                        zcta = zcta),
    zcta = ref_area_from_zcta(zcta),
    geoid =
      ref_area_from_geoid(geoid, geography, year, dataset, tidycensus_calls),
    ref_area_from_sc(state, county, geography, year, dataset, tidycensus_calls)
  )
}




ref_area_from_zcta <- function(zcta) {
  if (!is.null(zcta)) {
    zcta <- trimws(zcta)
    if (
      length(zcta) == 0L ||
      any(is.na(zcta)) ||
      !all(stringr::str_detect(zcta, "^\\d{1,5}$"))
    ) {
      stop(
        "zcta argument must be a character vector of digits,",
        "\neach between 1 and 5 characters long",
        call. = FALSE
      )
    }
  }
  
  # Validates non-NULL input to zcta and creates reference area (ref_area). It
  # must contain one or more strings of 1-5 digits.
  
  list(
    geoid        = NULL,
    geo_length   = NULL,
    state_county = dplyr::tibble(state = list(NULL), county = list(NULL)),
    zcta         = zcta
  )
  
}



ref_area_from_sc <- function(state, 
                             county, 
                             geography,
                             year,
                             dataset, 
                             tidycensus_calls,
                             single_location) {
  
  # Creates ref_area from user-inputted state and county.
  if (!is.null(county) && length(state) != 1L) {
    stop(
      "If supplying counties, exactly one state must be provided",
      "\nIn order to supply specific counties in multiple states,",
      "\nuse get_geoids() to get their GEOIDs and pass them to the",
      "geoid argument in this function.",
      call. = FALSE
    )
  }
  
  # This is a tibble with columns "state" and "county". It is constructed so
  # that the contents of each row can be successfully passed to the
  # corresponding "state" and "county" arugments in tidycensus::get_acs() or
  # tidycensus::get_decennial().
  state_county <-
    
    if (any(c("state", "county") == geography)) {
      
      # If geography is "state" or "county", the state and county args are
      # passed as-is to the tidycensus function(s).
      dplyr::tibble(state = list(state), county = list(county))
      
    } else if (is.null(county)) {
      # If geography = "tract" or smaller and county = NULL, preliminary
      # call(s) to tidycensus must be performed in order to obtain all county
      # GEOIDs for all states. The exception is the special case described
      # below.
      
      if (geography == "tract" && dataset == "decennial" && year != 2010) {
        # In this special case, the tidycensus function(s) will be called once
        # per state. Also, state = NULL is a shortcut to "all 50 states plus
        # DC and Puerto Rico")
        dplyr::tibble(
          state = if (is.null(state)) c(censusapi::fips, "72") else state
        )
      } else {
        sc_from_preliminary_call(tidycensus_calls, state)
      }
      
    } else {
      # If geography = "tract" or smaller and county was not NULL, the
      # tidycensus function(s) will be called once per county.
      dplyr::tibble(state = state, county = county)
    }
  
  list(
    geoid = NULL,
    geo_length = NULL,
    state_county = state_county,
    zcta = NULL
  )
}



sc_from_preliminary_call <- function(tidycensus_calls, state) {
  
  # Obtains a single character vector of all county GEOIDs for all states in the
  # "state" argument
  county_geoids <- county_geoids_from_state(tidycensus_calls, state)
  
  # Turns the character vector into a proper state_county object (i.e., a tibble
  # with columns "state" and "county").
  sc_from_county_geoids(county_geoids)
}



county_geoids_from_state <- function(tidycensus_calls, state) {
  
  message("\nPreliminary tidycensus call beginning...")
  
  # The preliminary tidycensus call is performed by modifying the tidycensus
  # call skeleton that happens earlier in get_adi(). It ends up requesting data
  # on a single arbitrary variable at the county level for all user-inputted
  # states.
  counties <-
    tidycensus_calls[[1L]] %>% 
    rlang::call_modify(
      geography = "county",
      variables = .$variables[1L],
      geometry = FALSE,
      shift_geo = FALSE,
      state = state
    ) %>% 
    eval() 
  
  # The data requested in the "variables" argument is not actually even saved;
  # only the column of GEOIDs is returned.
  counties[["GEOID"]]
}




sc_from_county_geoids <- function(county_geoids) {
  
  # Converts a character vector of 5+ digit GEOIDs into a proper
  # state_county object (i.e., a tibble with columns "state" and "county"). The
  # tidycensus functions' "state" argument accepts two-digit GEOIDs of states,
  # and their "county" argument accepts 3-digit county FIPS codes (see
  # ?tidycensus::get_acs) that are often seen concatenated onto the end of the
  # 2-digit state GEOID to make a 5-digit county GEOID (e.g., "39" (Ohio) +
  # "035" (Cuyahoga County) = "39035" (Cuyahoga County)).
  
  dplyr::tibble(
    state  = substr(county_geoids, 1L, 2L),
    county = substr(county_geoids, 3L, 5L)
  )
}




ref_area_from_geoid <- function(geoid,
                                geography, 
                                year, 
                                dataset, 
                                tidycensus_calls) {
  
  # Validates user input to geoid and constructs a state_county object (tibble
  # with columns "state" and "county") from it.
  
  geoid <- validate_geoid(geoid)
  
  # Get number of characters in a GEOID of user-inputted geography level. This
  # is used later when trimming tidycensus's results to only include areas in
  # the user-inputted reference area.
  geo_length <- validate_geo_length(geography, geoid)
  
  state_county <-
    sc_from_geoid(geoid, geography, year, dataset, tidycensus_calls)
  
  list(
    geoid = geoid,
    geo_length = geo_length,
    state_county = state_county,
    zcta = NULL
  )
}


validate_single_geoid <- function(geoid) {
  geoid <- validate_single_string(geoid, what = "geoid")
  if (!stringr::str_detect(geoid, "^\\d{2,12}$") ||
      !any(c(2L, 5L, 11L, 12L) == nchar(geoid))) {
    stop("geoid must be a single string consisting of 2, 5, 11, or 12 digits",
         call. = FALSE)
  }
  geoid
}


validate_geoid <- function(geoid) {
  
  geoid <- stringr::str_trim(geoid)
  
  # user_geoids must be a character vector with each element containing
  # exactly 2, 5, 11, or 12 digits.
  if (length(geoid) == 0L || any(is.na(geoid)) ||
      !all(stringr::str_detect(geoid, "^(\\d{2}|\\d{5}|\\d{11,12})$"))) {
    stop(
      "\nEach element of geoid must have exactly 2, 5, 11, or 12 digits,",
      "\nsignifying states, counties, tracts, and block groups, ",
      "respectively.\nDon't forget leading zeros.",
      call. = FALSE
    )
  }
  
  geoid
}



validate_single_string <- function(x, what, null_ok = FALSE) {
  if (null_ok && is.null(x)) {
    return(x)
  }
  if (!rlang::is_string(x) || !nchar(x)) {
    stop(what, " must be a single nonempty string", call. = FALSE)
  }
  x
}


validate_geo_length <- function(geography, geoid) {
  
  geo_length <-
    switch(
      geography, 
      block         = 15L,
      "block group" = 12L,
      tract         = 11L,
      county        =  5L,
      state         =  2L,
      stop(
        'geography does not match any of:\n',
        'c("block", "block group", "tract", "county", "state")',
        call. = FALSE
      )
    )
  
  if (max(nchar(geoid)) > geo_length) {
    # Warns user if they specified in the "geography" argument a level of
    # geography larger than any of the GEOIDs in the geoid argument.
    warning(
      'One or more geoids are more granular than geography = "', geography,
      '"\nThe ', geography, " in which these elements dwell will be used.",
      call. = FALSE
    )
  }
  
  geo_length
}




sc_from_geoid <- function(geoid,
                          geography,
                          year,
                          dataset,
                          tidycensus_calls) {
  
  # The state GEOIDs of each element in "geoid".
  first_two <- substr(geoid, 1L, 2L)
  
  if (any(c("state", "county") == geography)) {
    
    # If the user specified a geography of "state" or "county", the tidycensus
    # function(s) will be called once with the state argument containing a
    # character vector of all states GEOIDs represented in the geoid argument.
    dplyr::tibble(state = list(unique(first_two)))
    
  } else if (geography == "tract" && dataset == "decennial" && year != 2010) {
    
    # In this special case, the tidycensus function(s) will be called once per
    # unique state represented in the geoid argument.
    dplyr::tibble(state = unique(first_two))
    
  } else {
    
    # If the user specified a geography of "tract" or smaller, the tidycensus
    # function(s) will be called once per unique county represented in the geoid
    # argument.
    
    state_geoids_lgl <- geoid == first_two
    
    # If any elements in the "geoid" argument were state GEOIDs (i.e., 2
    # digits), they are passed to a preliminary tidycensus call in order to
    # obtain all their county GEOIDs. All the state GEOIDs are then removed, and
    # this list of county GEOIDs is appended.
    if (any(state_geoids_lgl)) {
      geoid <-
        c(
          geoid[!state_geoids_lgl],
          county_geoids_from_state(
            tidycensus_calls = tidycensus_calls,
            state = unique(geoid[state_geoids_lgl])
          )
        )
    }
    
    # All geoids are truncated to only include their 5-digit county geoids.
    county_geoids <- geoid %>% substr(1L, 5L) %>% unique()
    
    # Turns the character vector into a proper state_county object (i.e., a
    # tibble with columns "state" and "county").
    sc_from_county_geoids(county_geoids)
    
  }
}




validate_lon_lat <- function(lon_lat) {
  lon_lat <- as.double(lon_lat)
  if (!is.numeric(lon_lat) || length(lon_lat) != 2L ||
      !all(is.finite(lon_lat))) {
    stop("center must be coercible to a double vector of length 2,",
         "\nand both elements must be finite numbers", call. = FALSE)
  }
  lon_lat
}




validate_dissim_colnames <- function(dissimilarity_measure_name,
                                     sampling_weight_name,
                                     data) {
  if (is.null(dissimilarity_measure_name)) {
    if (is.null(sampling_weight_name)) {
      stop("Can't make both dissimilarity_measure_name and ",
           "sampling_weight_name NULL", call. = FALSE)
    }
  } else {
    if (!rlang::is_string(dissimilarity_measure_name) ||
        !nchar(dissimilarity_measure_name)) {
      stop("dissimilarity_measure_name must be a single, nonmissing, ",
           "nonempty string (or NULL to omit it in the output)", call. = FALSE)
    }
    if (any(names(data) == dissimilarity_measure_name)) {
      stop("A column named ", dissimilarity_measure_name,
           " already exists in data.",
           "\nEnter a different value for dissimilarity_measure_name",
           call. = FALSE)
    }
  }
  
  if (!is.null(sampling_weight_name)) {
    if (!rlang::is_string(sampling_weight_name) ||
        !nchar(sampling_weight_name)) {
      stop("sampling_weight_name must be a single, nonmissing, ",
           "nonempty string (or NULL to omit it in the output)", call. = FALSE)
    }
    if (any(names(data) == sampling_weight_name)) {
      stop("A column named ", sampling_weight_name,
           " already exists in data.",
           "\nEnter a different value for sampling_weight_name",
           call. = FALSE)
    }
  }
}


check_for_packages <- function(x) {
  check <- vapply(x, requireNamespace, FUN.VALUE = logical(1L), quietly = TRUE)
  if (!all(check)) {
    stop("\nThe following packages must be installed to use this function:",
         "\ninstall.packages(", deparse(x[!check]), ")", call. = FALSE)
  }
}