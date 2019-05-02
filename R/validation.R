
validate_location <- function(geoid, state, county, geography, zcta, ...) {
  
  dots_names <- names(eval(substitute(alist(...))))

  # If the user used "states", "counties", or "geoids" instead of "state",
  # "county", and "geoid", the function will noisily grab those values.
  if (any(dots_names == "states") && is.null(state)) {
    warning('Using value of "states" argument for "state" argument')
    state <- list(...)$states
  }
  if (any(dots_names == "counties") && is.null(county)) {
    warning('Using value of "counties" for "county" argument')
    county <- list(...)$counties
  }
  if (any(dots_names == "geoids") && is.null(geoid)) {
    warning('Using value of "geoids" argument for "geoid" argument')
    geoid <- list(...)$geoids
  }
  
  if (geography == "zip code tabulation area") {
    
    zcta <- validate_zcta(zcta, state, county, geoid)
    
    ref_area <-
      list(
        geoid        = NULL,
        geo_length   = NULL,
        state_county = list(list(state = NULL, county = NULL)),
        zcta         = zcta
      )
    
  } else if (is.null(geoid)) {
    
    if (!is.null(county) && length(state) != 1) {
      stop("If supplying counties, exactly one state must be provided")
    }
      
    ref_area <-
      list(
        geoid        = NULL,
        geo_length   = NULL,
        state_county = list(list(state = state, county = county)),
        zcta         = NULL
      )
    
  } else {
    
    if (!is.null(state) || !is.null(county)) {
      stop("Can't supply both geoid and state/county")
    }
    
    ref_area <- validate_geoid(geoid, geography)
  }
  
  ref_area
}


validate_geoid <- function(geoid, geography) {
  
  # Trim whitespace
  geoid <- stringr::str_trim(geoid)
  
  # user_geoids must be a vector of strings of digits between 2 and 12
  # characters. Otherwise, an error is thrown.
  if (
    length(geoid) == 0 ||
    any(is.na(geoid)) ||
    !all(stringr::str_detect(geoid, "^(\\d{2}|\\d{5}|\\d{11,12}|\\d{15})$"))
  ) {
    stop(
      "\ngeoid must have exactly 2, 5, 11, 12, or 15 digits\n",
      "signifying states, counties, tracts, block groups, and blocks,",
      "respectively.\nDon't forget leading zeros."
    )
  }
  
  geo_length <-
    dplyr::case_when(
      geography == "block"       ~ 15L,
      geography == "block group" ~ 12L,
      geography == "tract"       ~ 11L,
      geography == "county"      ~  5L,
      geography == "state"       ~  2L
    )
  
  
  if (max(nchar(geoid)) > geo_length) {
    warning(
      '"One or more geoids are more granular than geography = "', geography,
      '"\nThe ', geography, " in which these elements dwell will be used."
    )
  }
  
  state_county <- sc_from_geoid(geoid)
  
  list(
    geoid        = geoid,
    geo_length   = geo_length,
    state_county = state_county,
    zcta         = NULL
  )
}


# @importFrom rlang .data
sc_from_geoid <- function(geoid) {
  
  # Extracts state and county FIPS codes from the user-entered GEOIDs.
  # The filter portion removes any explicitly named counties in a state that the
  # user had also entered by itself.
  # 
  # Example: the user entered geoid = c("39035", "01003", "39", "01001")
  # Before the filter: 
  #   ~state   ~county
  #     "39"     "035"
  #     "01"     "003"
  #     "39"        ""
  #     "01"     "001"
  # After the filter:
  #   ~state   ~county
  #     "01"     "003"
  #     "39"        ""
  #     "01"     "001"
  # If this filtration did not occur, only the explicitly specified counties'
  # ADIs would be returned, and the rest in the state would be excluded.
  
  first_two <- substr(geoid, 1L, 2L)
  
  lapply(
    unique(first_two),
    function(state) {
      
      county <- unique(substr(geoid[first_two == state], 3L, 5L))
      
      if (any(county == "")) {
        county = NULL
      }
      
      list(state = state, county = county)
    }
  )
  
  # user_counties <-
  #   tibble::tibble(
  #     state_fips  = stringr::str_sub(geoid, 1L, 2L),
  #     county_fips = stringr::str_sub(geoid, 3L, 5L)
  #   ) %>% 
  #   dplyr::distinct(.data$state_fips, .data$county_fips) %>% 
  #   dplyr::filter(
  #     .data$county_fips == "" |
  #       .data$county_fips != "" &
  #       !(.data$state_fips %in% .data$state_fips[.data$county_fips == ""])
  #   )
  # 
  # lapply(
  #   unique(user_counties$state_fips),
  #   function(state) {
  #     county <- 
  #       dplyr::filter(
  #         user_counties,
  #         .data$state_fips == state & .data$county_fips != "")$county_fips
  #     
  #     if (length(county) == 0) {
  #       county <- NULL
  #     }
  #     
  #     list(state = state, county = county)
  #   }
  # )
}



validate_zcta <- function(zcta, state, county, geoid) {
  
  if (!is.null(state) || !is.null(county) || !is.null(geoid)) {
    stop(
      'If geography = "zcta",',
      ' then state, county, and geoid args must be NULL.'
    )
  }
  
  if (is.null(zcta)) {
    NULL
  } else {
    zcta <- trimws(zcta)
    
    if (
      length(zcta) == 0 ||
      any(is.na(zcta)) ||
      !all(stringr::str_detect(zcta, "^\\d{1,5}$"))
    ) {
      stop(
        "zcta argument must be a character vector of digits,",
        "\neach between 1 and 5 characters long"
      )
    }
    
    zcta
  }
}


validate_tidycensus_args <- function(args, fn) {
  args[
    names(args) %in%
      c(methods::formalArgs(fn), "cb", "resolution", "starts_with")
  ]
}
