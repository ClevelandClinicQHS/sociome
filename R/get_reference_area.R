# Sends geoid and geography to the function get_reference_area, which
# validates the GEOIDs in geoid and returns a ref_area-class object, which
# contains all the data needed to produce the ADIs specified by the user.
# See the comments in get_reference_area.R for more information on this
# object.
#get_reference_area <- function(user_geoids = NULL, geography = NULL) {
validate_geoid <- function(geoid, geography) {
  # If user_geoids is NULL, all locations in the US are used.
  # if(is.null(user_geoids)) {
  # 
  #   user_blk_grps <- fips_table
  # 
  #   # If geography is also NULL, "tract" is chosen as the level of geography
  #   if(is.null(geography)) {
  #     geography <- "tract"
  #   }
  # }

  # Otherwise, the user_geoids are parsed, wittling the table of all US
  # locations down to only the ones needed.
  #else {
    # Trim whitespace
    geoid <- stringr::str_trim(geoid)

    # user_geoids must be a vector of strings of digits between 2 and 12
    # characters. Otherwise, an error is thrown.
    if(!checkmate::test_character(x           = geoid,
                                  pattern     = "^(\\d{2}|\\d{5}|\\d{11,12})$",
                                  min.len     = 1,
                                  any.missing = FALSE)) {
      stop("geoid must have exactly 2, 5, 11, or 12 digits\n",
           "signifying states, counties, tracts, and block groups, ",
           "respectively.\nDon't forget leading zeros.")
    }

    # number of characters of each geoid is counted
    #geoid_length <- nchar(geoid)

    # # If any geoid does not have 2, 5, 11, or 12 characters, an error is thrown.
    # if(!all(geoid_length %in% c(2, 5, 11, 12))) {
    #   stop("Incompatible geoid detected. See help('get_adi') for details")
    # }

    # # If the user did not supply a geography, it chooses the most specific
    # # level of geography present in user_geoids.
    # if(is.null(geography)) {
    #   geography <- dplyr::case_when(
    #     12 %in% geoid_length ~ "block group",
    #     11 %in% geoid_length ~ "tract",
    #      5 %in% geoid_length ~ "county",
    #      2 %in% geoid_length ~ "state"
    #   )
    # }

    # Otherwise, warns user if the most granular GEOID in user_geoids is more
    # granular than the user-specified level of geography
    #else {
      # geography_granularity <- 
    if(max(nchar(geoid)) > dplyr::case_when(geography == "block group" ~ 12,
                                            # geography == "block"       ~ 15,
                                            geography == "tract"       ~ 11,
                                            geography == "county"      ~  5,
                                            geography == "state"       ~  2)) {
      warning(paste0("one or more geoid elements are more granular than ",
                     "geography.\nSee help('get_adi')"))
    }
    #}

    # Selects all rows of fips_table that contain any of the geoids in
    # user_geoids, and then unique() is used to remove duplicate rows.
    user_blk_grps <-
      geoid %>% 
      purrr::map_dfr(
        function(geoid)
          fips_table[as.logical(rowSums(geoid == fips_table[,1:4])),]) %>%
      unique()
  #}

  ##############################################################################
  # Sets up the ref_area-class object, which is a list of 3 elements:
  #   1. ref_geoids
  #      The actual reference area: a character vector containing all the GEOIDs
  #      in the reference area at the user-specified level of geography.
  #   2. geography
  #      Character string indicating the user-specified level of geography
  #      (i.e., one of "state", "county", "tract", or "block group")
  #   3. state_county
  #      A list. Each element of this list is itself a list of 2 elements named
  #      "state" and "county".
  #      
  #      If geography is equal to "state" or "county", there will only be one
  #      list of 2, valued thusly:
  #        state
  #           If the reference area is the entire country:
  #             NULL. This is to accomodate how tidycensus::get_acs handles the
  #             argument shift_geo (requires that its "state" argument is NULL).
  #           Else:
  #             A character vector of the GEOIDs of the states that encompass a
  #             portion of the reference area.
  #        county
  #           NULL
  #      
  #      If geography is "tract" or "block group", the number of lists of 2 will
  #      be equal to the number of states that encompass a portion of the
  #      reference area. Each list of 2 is valued thusly:
  #        state
  #           A single character string containing the two-character GEOID of
  #           one of the states that encompass the reference area.
  #        county
  #           A character string vector of the three-character GEOIDs of the
  #           counties in that state that encompass a portion of the reference
  #           area.
  
  user_states <- unique(user_blk_grps$state)
    
  ref_area <- list(
    ref_geoids   = unique(user_blk_grps[[geography]]),
    geography    = geography,
    state_county = NULL)
    
  if(geography %in% c("tract", "block group", "block")) {
    ref_area$state_county <-
      lapply(X   = user_states,
             FUN = function(user_state) {
               list(state  = user_state,
                    county =
                      unique(dplyr::filter(user_blk_grps,
                                           .data$state ==
                                             user_state)$short_county))
             })
  }
  else if(geography == "state" || geography == "county") {
    
    ref_area$state_county <- list(list(state = unique(user_blk_grps$state),
                                       county = NULL))
    
    # if(!identical(user_blk_grps, fips_table)) {
    #   ref_area[["state_county"]][[1]][["state"]] <- unique(user_blk_grps$state)
    # }
    
  }
  else {
    stop('geography must be "state", "county", "tract", or "block group"')
  }
  ##############################################################################
  
  class(ref_area) <- "ref_area"

  return(ref_area)
}
