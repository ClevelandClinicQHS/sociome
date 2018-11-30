
validate_geoid <- function(geoid, state, county, ...) {
  
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
  
  if(!is.null(county)) {
    
    # Throws error if user supples counties but doesn't supply exactly one state
    if(length(state) != 1) {
      stop("If supplying counties, exactly one state must be provided")
    }
    
    # Throws error if user supplies county, state, and geoid
    if(!is.null(geoid)) {
      stop("If supplying geoid, state and county must be NULL")
    }
    
    # Validates user-supplied state and coerces it into its two-digit GEOID
    state <- validate_state(state)
    
    # Validates user-supplied county values and populates geoid with the
    # counties' five-digit GEOIDs
    geoid <-
      unique(sapply(county,
                    function(x) {
                      paste0(state,
                             validate_county(state = state, county = x))}))
  }
  
  else if(!is.null(state)) {
    
    # Throws error if user supplies both state and geoid
    if(!is.null(geoid)) {
      stop("If supplying geoid, state and county must be NULL")
    }
    
    # Otherwise, populates geoid with states' two-digit geoid.
    else {
      geoid <- unique(sapply(state, validate_state))
    }
    
  }
  
  return(geoid)
}

###############################################################################
# The functions below are copied verbatim or nearly verbatim from the tidycensus
# package's internal functions with the same names. The license for this code is
# as follows:
# License: MIT + the following LICENSE (which can also be found at
# https://cran.r-project.org/web/packages/tidycensus/LICENSE):
#
# YEAR: 2017
# COPYRIGHT HOLDER: Kyle Walker
###############################################################################
validate_state <- function(state, .msg = interactive())
{
  if (is.null(state))
    return(NULL)
  state <- tolower(stringr::str_trim(state))
  if (grepl("^[[:digit:]]+$", state)) {
    state <- sprintf("%02d", as.numeric(state))
    if (state %in% fips_state_table$fips) {
      return(state)
    }
    else {
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        message(
          sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                  state, state_sub,
                  fips_state_table[fips_state_table$fips ==
                                     state_sub, "name"]),
          call. = FALSE)
        return(state_sub)
      }
      else {
        warning(
          sprintf("'%s' is not a valid FIPS code or state name/abbreviation",
                  state), call. = FALSE)
        return(NULL)
      }
    }
  }
  else if (grepl("^[[:alpha:]]+", state)) {
    if (nchar(state) == 2 && state %in% fips_state_table$abb) {
      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$abb == state,
                                         "fips"], toupper(state)))
      return(fips_state_table[fips_state_table$abb == state,
                              "fips"])
    }
    else if (nchar(state) > 2 && state %in% fips_state_table$name) {
      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$name == state,
                                         "fips"], simpleCapSO(state)))
      return(fips_state_table[fips_state_table$name ==
                                state, "fips"])
    }
    else {
      warning(
        sprintf("'%s' is not a valid FIPS code or state name/abbreviation",
                state), call. = FALSE)
      return(NULL)
    }
  }
  else {
    warning(sprintf("'%s' is not a valid FIPS code or state name/abbreviation",
                    state), call. = FALSE)
    return(NULL)
  }
}

simpleCapSO <- function(x)
{
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1, 1)), substring(s, 2), collapse = " ")
}

validate_county <- function (state, county, .msg = interactive())
{
  if (is.null(state) || is.null(county))
    return(NULL)
  state <- validate_state(state)
  county_table <-
    tidycensus::fips_codes[tidycensus::fips_codes$state_code == state,
                           ]
  if (grepl("^[[:digit:]]+$", county)) {
    county <- sprintf("%03d", as.numeric(county))
    if (county %in% county_table$county_code) {
      return(county)
    }
    else {
      warning(sprintf("'%s' is not a valid FIPS code for counties in %s",
                      county, county_table$state_name[1]), call. = FALSE)
      return(NULL)
    }
  }
  else if ((grepl("^[[:alpha:]]+", county))) {
    county_index <- grepl(sprintf("^%s", county), county_table$county,
                          ignore.case = TRUE)
    matching_counties <- county_table$county[county_index]
    if (length(matching_counties) == 0) {
      warning(sprintf("'%s' is not a valid name for counties in %s",
                      county, county_table$state_name[1]), call. = FALSE)
      return(NULL)
    }
    else if (length(matching_counties) == 1) {
      if (.msg)
        message(sprintf("Using FIPS code '%s' for '%s'",
                        county_table[county_table$county == matching_counties,
                                     "county_code"], matching_counties))
      return(county_table[county_table$county == matching_counties,
                          "county_code"])
    }
    else if (length(matching_counties) > 1) {
      ctys <- format_vec(matching_counties)
      warning("Your county string matches ", ctys,
              " Please refine your selection.", call. = FALSE)
      return(NULL)
    }
  }
}

format_vec <- function(vec)
{
  out <- paste0(vec, ", ")
  l <- length(out)
  out[l - 1] <- paste0(out[l - 1], "and ")
  out[l] <- gsub(", ", ".", out[l])
  return(paste0(out, collapse = ""))
}


validate_tidycensus_args <- function(args, fn) {
  args[names(args) %in% c(methods::formalArgs(fn),
                          "cb", "resolution", "starts_with")]
}


validate_type <- function(type, data) {
  
  if(!checkmate::test_character(type,
                                pattern     = "(^sf3$|^acs(1|3|5)$)",
                                any.missing = FALSE,
                                len         = 1,
                                null.ok     = TRUE)) {
    stop('type must be either "acs1", "acs3", "acs5", or "sf3"')
  }
  
  if(is.null(type)) {
    
    acs_test <- tidyselect::matches(match       = "(B|C)\\d{5}_\\d{3}E",
                                    ignore.case = FALSE,
                                    vars        = names(data))
    if(length(acs_test) > 0) {
      type <- "sf3"
    }
    else {
      decennial_test <- tidyselect::matches(match       = "(H|P)\\d{6}",
                                            ignore.case = FALSE,
                                            vars        = names(data))
      if(length(decennial_test) > 0) {
        type <- "acs"
      }
      else {
        stop("Unable to determine whether data is from ACS or decennial ",
             "census.\nCheck the variable names.")
      }
    }
  }
  
  return(type)
}

validate_data <- function(data, type) {
  
  if(type == "sf3") {
    vars <- decennial_vars$variable
  }
  else {
    vars <- paste0(acs_vars$variable, "E")
  }
  
  if(!all(vars %in% colnames(data))) {
    missing_vars <- vars[!(vars %in% colnames(data))]
    stop(paste(c("The following variables are missing from data:",
                 missing_vars), collapse = " "))
  }
  
  if(nrow(data) < 30) {
    warning("\n\nCalculating ADI values from fewer than 30 locations.\nIt is ",
            "recommended to add more in order to obtain trustworthy results.\n")
  }
  
  return(vars)
}