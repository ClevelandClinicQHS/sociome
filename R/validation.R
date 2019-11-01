
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



validate_year <- function(year) {
  
  if (!rlang::is_scalar_integerish(year)) {
    stop("year must be a single number")
  }
  
  as.numeric(year)
}



validate_dataset <- function(dataset = c("acs5", "acs3", "acs1", "decennial"),
                             year,
                             geography) {
  
  dataset <- match.arg(dataset)
  
  if (dataset == "decennial") {
    
    if (!any(c(1990, 2000, 2010) == year)) {
      stop(
        'When setting dataset = "decennial", year must be 1990 or 2000.',
        "\n(The 2010 Decennial Census did not include the long-form survey",
        "\nthat gathered the data needed to produce an ADI.",
        '\nUse dataset = "acs5" for 2010.)'
      )
    }
    
    if (geography == "zip code tabulation area") {
      stop(
        'geography = "zip code tabulation area" not supported',
        '\nfor the decennial census data.',
        '\nTry dataset = "acs5" (or "acs3" or "acs1")'
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




validate_location <- function(geoid,
                              state, 
                              county, 
                              zcta, 
                              geography,
                              year,
                              dataset,
                              exec_arg_tibble) {
  if (geography == "zip code tabulation area") {
    if (!is.null(state) || !is.null(county) || !is.null(geoid)) {
      stop(
        'If geography = "zcta",',
        ' then state, county, and geoid args must be NULL.'
      )
    }
    ref_area_from_zcta(zcta)
  } else {
    if (!is.null(zcta)) {
      stop(
        "If supplying something to zcta argument then",
        '\ngeography = "zip code tabulation area" is required.'
      )
    }
    if (is.null(geoid)) {
      ref_area_from_sc(state, county, geography, year, dataset, exec_arg_tibble)
    } else {
      if (!is.null(state) || !is.null(county)) {
        stop("Can't supply both geoid and state/county")
      }
      ref_area_from_geoid(geoid, geography, year, dataset, exec_arg_tibble)
    }
  }
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
        "\neach between 1 and 5 characters long"
      )
    }
  }
  
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
                             exec_arg_tibble) {
  
  if (!is.null(county) && length(state) != 1L) {
    stop(
      "If supplying counties, exactly one state must be provided",
      "\nIn order to supply specific counties in multiple states,",
      "\nuse get_geoids() to get their GEOIDs and pass them to the",
      "geoid argument in this function."
    )
  }
  
  state_county <-
    if (any(c("state", "county") == geography)) {
      dplyr::tibble(state = list(state), county = list(county))
    } else if (is.null(county)) {
      
      if (geography == "tract" && dataset == "decennial" && year != 2010) {
        dplyr::tibble(
          state = if (is.null(state)) c(censusapi::fips, "72") else state
        )
      } else {
        sc_from_preliminary_call(exec_arg_tibble, state)
      }
      
    } else {
      dplyr::tibble(state = state, county = county)
    }
  
  list(
    geoid = NULL,
    geo_length = NULL,
    state_county = state_county,
    zcta = NULL
  )
}



sc_from_preliminary_call <- function(exec_arg_tibble, state) {
  
  county_geoids <- county_geoids_from_state(exec_arg_tibble, state)
  
  sc_from_county_geoids(county_geoids)
}



county_geoids_from_state <- function(exec_arg_tibble, state) {
  
  exec_args <- purrr::map(exec_arg_tibble, 1L)
  
  exec_args$geography   <- "county"
  exec_args$variables   <- exec_args$variables[1L]
  exec_args$geometry    <- FALSE
  exec_args$shift_geo   <- FALSE
  exec_args$summary_var <- NULL
  exec_args$state       <- state
  
  counties <- do.call(exec_tidycensus, exec_args)
  
  counties$GEOID
}




sc_from_county_geoids <- function(county_geoids) {
  dplyr::tibble(
    state  = substr(county_geoids, 1L, 2L),
    county = substr(county_geoids, 3L, 5L)
  )
}




ref_area_from_geoid <- function(geoid,
                                geography, 
                                year, 
                                dataset, 
                                exec_arg_tibble) {
  geoid <- validate_geoid(geoid)
  
  geo_length <- validate_geo_length(geography, geoid)
  
  state_county <-
    sc_from_geoid(geoid, geography, year, dataset, exec_arg_tibble)
  
  list(
    geoid = geoid,
    geo_length = geo_length,
    state_county = state_county,
    zcta = NULL
  )
}



validate_geoid <- function(geoid) {
  
  geoid <- stringr::str_trim(geoid)
  
  # user_geoids must be a vector of strings of digits between 2 and 12
  # characters. Otherwise, an error is thrown.
  if (
    length(geoid) == 0L ||
    any(is.na(geoid)) ||
    !all(stringr::str_detect(geoid, "^(\\d{2}|\\d{5}|\\d{11,12})$"))
  ) {
    stop(
      "\nEach element of geoid must have exactly 2, 5, 11, or 12 digits,",
      "\nsignifying states, counties, tracts, and block groups, respectively.",
      "\nDon't forget leading zeros."
    )
  }
  
  geoid
}




validate_geo_length <- function(geography, geoid) {
  
  geo_length <-
    switch(
      geography, 
      "block"       = 15L,
      "block group" = 12L,
      "tract"       = 11L,
      "county"      =  5L,
      "state"       =  2L,
      stop(
        'geography does not match any of:\n',
        'c("block", "block group", "tract", "county", "state")'
      )
    )
  
  if (max(nchar(geoid)) > geo_length) {
    warning(
      'One or more geoids are more granular than geography = "', geography,
      '"\nThe ', geography, " in which these elements dwell will be used."
    )
  }
  
  geo_length
}




# @importFrom rlang .data
sc_from_geoid <- function(geoid, geography, year, dataset, exec_arg_tibble) {
  
  first_two <- substr(geoid, 1L, 2L)
  
  if (any(c("state", "county") == geography)) {
    
    dplyr::tibble(state = list(unique(first_two)))
    
  } else if (geography == "tract" && dataset == "decennial" && year != 2010) {
    
    dplyr::tibble(state = unique(first_two))
    
  } else {
    
    state_geoids_lgl <- geoid == first_two
    
    if (any(state_geoids_lgl)) {
      geoid <-
        c(
          geoid[!state_geoids_lgl],
          county_geoids_from_state(
            exec_arg_tibble = exec_arg_tibble,
            state = unique(geoid[state_geoids_lgl])
          )
        )
    }
    
    county_geoids <- geoid %>% substr(1L, 5L) %>% unique()
    
    sc_from_county_geoids(county_geoids)
    
  }
}



cross_args <- function(exec_arg_tibble, 
                       state_county, 
                       geography, 
                       year, 
                       dataset) {
  if (geography == "tract" && year == 2010 && dataset == "decennial") {
    dplyr::bind_rows(
      dplyr::tibble(
        !!!exec_arg_tibble[1L, ], # This is the decennial row
        state = unique(state_county$state)
      ),
      dplyr::tibble(
        !!!exec_arg_tibble[2L, ], # This is the acs row
        state = state_county$state,
        county = as.vector(state_county$county, mode = "list")
      )
    )
  } else {
    tidyr::crossing(exec_arg_tibble, state_county)
  }
}
