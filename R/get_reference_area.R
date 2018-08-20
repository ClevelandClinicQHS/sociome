get_reference_area <- function(user_geoids = NULL, geography = NULL) {

  # If user_geoids is NULL, all locations in the US are used.
  if(is.null(user_geoids)) {

    user_blk_grps <- fips_table

    # If geography is also NULL, "tract" is chosen as the level of
    # geography
    if(is.null(geography)) {
      geography <- "tract"
    }
  }

  # Otherwise, the user_geoids are parsed, wittling the table of all US
  # locations down to only the ones needed.
  else {
    # Trim whitespace
    user_geoids <- stringr::str_trim(user_geoids)

    # user_geoids must be a vector of strings of digits between 2 and 12
    # characters. Otherwise, an error is thrown.
    checkmate::assert_character(
      user_geoids, pattern = "^[[:digit:]]{2,12}$",
      min.len = 1, any.missing = FALSE)

    # number of characters of each geoid is counted
    geoid_length <- nchar(user_geoids)

    # If any geoid does not have 2, 5, 11, or 12 characters, an error is thrown.
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

    # Selects all rows of fips_table that contain any of the geoids in
    # user_geoids, and then unique() is used to remove duplicate rows.
    user_blk_grps <- user_geoids %>% 
      purrr::map_dfr(
        function(geoid)
          fips_table[as.logical(rowSums(geoid == fips_table[,1:4])),]) %>% 
      unique()
  }

  # Sets ups the ref_area-class object, which is a list of 3 elements:
  #   1. ref_geoids
  #      The actual reference area: a character vector containing all the GEOIDs
  #      in the reference area at the user-specified level of geography.
  #   2. geography
  #      Character string indicating the user-specified level of geography
  #      (i.e., one of "state", "county", "tract", or "block group")
  #   3. state_county
  #      A list. Each element of this list is a list of 2. Each list of 2
  #      contains:
  #         a) A character vector of one or more state GEOIDs
  #         b) If element (a) above contains more than one state, NULL.
  #            If element (a) contains exactly one state, a character vector of
  #              the three-character GEOIDs corresponding to the user-specified
  #              tracts/block groups.
  ref_area <- list(
    ref_geoids = unname(as.vector(unique(user_blk_grps[[geography]]))),
    geography = geography,
    state_county = NULL)
  
  if(geography == "tract" | geography == "block group"){
    ref_area[["state_county"]] <-
      lapply(unique(user_blk_grps$state),
             function(user_state)
               list(state  = user_state,
                    county =
                      unique(dplyr::filter(user_blk_grps,
                                           .data$state ==
                                             user_state)$short_county)))
  }
  else if(geography == "state" | geography == "county"){
    ref_area[["state_county"]] <-
      list(list(state = unique(user_blk_grps$state),
                county = NULL))
  }
  else {
    stop('geography must be "state", "county", "tract", or "block group"')
  }
  
  class(ref_area) <- "ref_area"

  return(ref_area)
}
