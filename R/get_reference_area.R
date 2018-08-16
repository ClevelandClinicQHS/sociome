get_reference_area <- function(user_geoids = NULL, geography = NULL) {

  # If user_geoids is NULL, the entire fips_table is assigned to user_blk_grps
  if(is.null(user_geoids)) {

    user_blk_grps <- fips_table

    # If geography is also NULL, "tract" is chosen as the level of
    # geography
    if(is.null(geography)) {
      geography <- "tract"
    }
  }

  # Otherwise, the user_geoids are parsed
  else {
    # Trim whitespace
    user_geoids <- stringr::str_trim(user_geoids)

    # user_geoids must be a vector of strings of digits between 2 and 12
    # characters
    checkmate::assert_character(
      user_geoids, pattern = "^[[:digit:]]{2,12}$",
      min.len = 1, any.missing = FALSE)

    geoid_length <- nchar(user_geoids)

    if(!all(geoid_length %in% c(2, 5, 11, 12))) {
      stop("Incompatible geoid detected. See help('get_adi') for details")
    }

    # If the user did not supply a geography, it chooses the most specific
    # level of geography present in user_geoids.
    if(is.null(geography)) {
      geography <- dplyr::case_when(
        12 %in% geoid_length ~ "block group",
        11 %in% geoid_length ~ "tract",
         5 %in% geoid_length ~ "county",
         2 %in% geoid_length ~ "state"
      )
    }

    # Otherwise, warns user if the most granular GEOID in user_geoids is more
    # granular than the user-specified level of geography
    else {
      geography_granularity <- dplyr::case_when(
        geography == "block group" ~ 12,
        geography == "tract" ~ 11,
        geography == "county" ~ 5,
        geography == "state" ~ 2
      )
      if(max(geoid_length) > geography_granularity) {
        warning("user-supplied GEOIDs more granular than user-supplied level of geography. See help('get_adi')")
      }
    }

    # Selects all rows of fips_table that have a number in common with
    # user_geoids
    user_blk_grps <-
      unique(purrr::map_dfr(
        user_geoids, function(x)
          fips_table[as.logical(rowSums(x == fips_table[,1:4])),]))
  }

  # Creates a ref_area-class object
  ref_area <- list(
    ref_geoids = unname(as.vector(unique(user_blk_grps[[geography]]))),
    geography = geography,
    state_county = NULL)
  
  if(geography == "tract" | geography == "block group"){
    ref_area[["state_county"]] <-
      lapply(unique(user_blk_grps$state),
             function(user_state)
               list(state  = user_state,
                    county = unique(dplyr::filter(user_blk_grps,
                                    state == user_state)$short_county)))
  }
  else if(geography == "state" | geography == "county"){
    ref_area[["state_county"]] <-
      list(list(state = unique(user_blk_grps$state),
                              county = NULL))
  }
  else {
    ref_area[["state_county"]] <-
      list(list(state = NULL, county = NULL))
  }
  
  class(ref_area) <- "ref_area"

  return(ref_area)
}
