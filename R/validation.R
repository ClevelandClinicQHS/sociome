
validate_location <- function(geoid, state, county, geography, ...) {
  
  dots_names <- names(eval(substitute(alist(...))))

  # If the user used "states", "counties", or "geoids" instead of "state",
  # "county", and "geoid", the function will quietly grab those values.
  if(is.null(state) && "states" %in% dots_names) {
    state <- list(...)$states
  }
  if(is.null(county) && "counties" %in% dots_names) {
    county <- list(...)$counties
  }
  if(is.null(geoid) && "geoids" %in% dots_names) {
    geoid <- list(...)$geoids
  }
  
  if(is.null(geoid)) {
    
    if(!is.null(county) && length(state) != 1) {
      stop("If supplying counties, exactly one state must be provided")
    }
      
    ref_area <- list(geoid        = NULL,
                     geo_length   = NULL,
                     state_county = list(list(state = state, county = county)))
  }
  else {
    
    if(!is.null(state) || !is.null(county)) {
      stop("Can't supply both geoid and state/county")
    }
    
    ref_area <- validate_geoid(geoid, geography)
  } 
  
  return(ref_area)
}


validate_geoid <- function(geoid, geography) {
  
  # Trim whitespace
  geoid <- stringr::str_trim(geoid)
  
  # user_geoids must be a vector of strings of digits between 2 and 12
  # characters. Otherwise, an error is thrown.
  if(
    !checkmate::test_character(
      x           = geoid,
      pattern     = "^(\\d{2}|\\d{5}|\\d{11,12}|\\d{15})$",
      min.len     = 1,
      any.missing = FALSE)) {
    stop("\ngeoid must have exactly 2, 5, 11, 12, or 15 digits\n",
         "signifying states, counties, tracts, block groups, and blocks,",
         "respectively.\nDon't forget leading zeros.")
  }
  
  geo_length <- dplyr::case_when(geography == "block"       ~ 15,
                                 geography == "block group" ~ 12,
                                 geography == "tract"       ~ 11,
                                 geography == "county"      ~  5,
                                 geography == "state"       ~  2)
  
  
  if(max(nchar(geoid)) > geo_length) {
    warning("One or more geoids are more granular than geography = ",
            geography, ".\nThe ", geography,
            " in which these elements dwell will be used.")
  }
  
  state_county <- sc_from_geoid(geoid)
  
  return(list(geoid        = geoid,
              geo_length   = geo_length,
              state_county = state_county))
}


#' @importFrom rlang .data
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
  user_counties <-
    tibble::tibble(state_fips  = stringr::str_sub(geoid, 1, 2),
                   county_fips = stringr::str_sub(geoid, 3, 5)) %>% 
    dplyr::distinct(.data$state_fips, .data$county_fips) %>% 
    dplyr::filter(
      .data$county_fips == "" |
        .data$county_fips != "" & !(.data$state_fips %in%
                                      .$state_fips[.$county_fips == ""]))
  
  lapply(
    unique(user_counties$state_fips),
    function(state) {
      county <- 
        dplyr::filter(
          user_counties,
          .data$state_fips == state & .data$county_fips != "")$county_fips
      
      if(length(county) == 0) {
        county <- NULL
      }
      
      list(state = state, county = county)
    })
}


validate_tidycensus_args <- function(args, fn) {
  args[names(args) %in% c(methods::formalArgs(fn),
                          "cb", "resolution", "starts_with")]
}
