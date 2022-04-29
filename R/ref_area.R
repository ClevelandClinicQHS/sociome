
ref_area <- function(geography,
                     state,
                     county,
                     zcta,
                     geoid,
                     year,
                     dataset,
                     partial_tidycensus_calls,
                     eval_insistently) {
  switch(
    geography,
    "zip code tabulation area" = ref_area_from_zcta(zcta),
    switch(
      determine_input_arg(geoid = geoid, state = state, county = county),
      geoid =
        ref_area_from_geoid(
          geoid = geoid,
          geography = geography, 
          year = year, 
          dataset = dataset, 
          partial_tidycensus_calls = partial_tidycensus_calls,
          eval_insistently = eval_insistently
        ), 
      ref_area_from_sc(
        state = state,
        county = county, 
        geography = geography, 
        year = year, 
        dataset = dataset, 
        partial_tidycensus_calls = partial_tidycensus_calls,
        eval_insistently = eval_insistently
      )
    )
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
                             partial_tidycensus_calls,
                             eval_insistently) {
  
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
        sc_from_preliminary_call(
          partial_tidycensus_calls,
          state,
          eval_insistently
        )
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



sc_from_preliminary_call <- function(partial_tidycensus_calls, 
                                     state,
                                     eval_insistently) {
  
  # Obtains a single character vector of all county GEOIDs for all states in the
  # "state" argument
  county_geoids <-
    county_geoids_from_state(partial_tidycensus_calls, state, eval_insistently)
  
  # Turns the character vector into a proper state_county object (i.e., a tibble
  # with columns "state" and "county").
  sc_from_county_geoids(county_geoids)
}



county_geoids_from_state <- function(partial_tidycensus_calls, 
                                     state, 
                                     eval_insistently) {
  message("\nPreliminary tidycensus call beginning...")
  
  # The preliminary tidycensus call is performed by modifying the tidycensus
  # call skeleton that happens earlier in get_adi(). It ends up requesting data
  # on a single arbitrary variable at the county level for all user-inputted
  # states.
  counties <-
    partial_tidycensus_calls[[1L]] %>% 
    rlang::call_modify(
      geography = "county",
      variables = .$variables[1L],
      geometry = FALSE,
      shift_geo = rlang::zap(),
      state = state
    ) %>% 
    eval_insistently()
  
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
                                partial_tidycensus_calls,
                                eval_insistently) {
  
  # Validates user input to geoid and constructs a state_county object (tibble
  # with columns "state" and "county") from it.
  
  geoid <- validate_geoid(geoid)
  
  # Get number of characters in a GEOID of user-inputted geography level. This
  # is used later when trimming tidycensus's results to only include areas in
  # the user-inputted reference area.
  geo_length <- validate_geo_length(geography, geoid)
  
  state_county <-
    sc_from_geoid(
      geoid,
      geography,
      year,
      dataset,
      partial_tidycensus_calls,
      eval_insistently
    )
  
  list(
    geoid = geoid,
    geo_length = geo_length,
    state_county = state_county,
    zcta = NULL
  )
}

sc_from_geoid <- function(geoid,
                          geography,
                          year,
                          dataset,
                          partial_tidycensus_calls,
                          eval_insistently) {
  
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
            partial_tidycensus_calls = partial_tidycensus_calls,
            state = unique(geoid[state_geoids_lgl]),
            eval_insistently = eval_insistently
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
