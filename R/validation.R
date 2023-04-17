
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
    
    if (!any(c(1990, 2000, 2010, 2020) == year)) {
      stop(
        'When setting dataset = "decennial", ',
        'year must be 1990, 2000, 2010, or 2020.',
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



determine_input_arg <- function(geoid = NULL, state = NULL, county = NULL) {
  args <- c("geoid"[!is.null(geoid)], "state"[!is.null(state)],
            "county"[!is.null(county)])
  
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
    }
  )
  
  stop("Enter only one of:\ngeoid,\nor state/county", call. = FALSE)
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