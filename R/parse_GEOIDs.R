
#' parse_geoids
#'
#' Processes a vector of GEOIDs, coercing them to the most specific level of
#' geography present, and returns a list of class "geoids".
#'
#' @param user_geoids a character vector of GEOIDs. They don't all have to
#'   represent the same level of geography
#'
#' @return A list of class "geoids", which contains the original vector of
#'   GEOIDs and a string indicating the level of geography that the
#'   GEOIDs represent (one of "state", "county", "tract", or "block group").
# @export

parse_geoids <- function(user_geoids) {

  # Trim whitespace
  user_geoids <- stringr::str_trim(user_geoids)

  # user_geoids must be a vector of strings of digits between 2 and 12 characters
  checkmate::assert_character(
    user_geoids, pattern = "^[[:digit:]]{2,12}$", min.len = 1, any.missing = FALSE)

  geoid_length <- unique(nchar(user_geoids))

  # Checks to see if all elements of domain have the same number of characters
  # if(length(geoid_length) > 1) {
  #   stop("All GEOIDs must have the same number of digits")
  # }

  if(!all(geoid_length %in% c(2, 5, 11, 12))) {
    stop("All GEOIDs must contain either 2 digits (states), 5 digits (counties), 11 digits (census tracts), or 12 digits (census block groups)")
  }

  # Chooses the most specific level of geography present in user_geoids
  geography_level <- dplyr::case_when(
    12 %in% geoid_length ~ "block_group",
    11 %in% geoid_length ~ "tract",
     5 %in% geoid_length ~ "county",
     2 %in% geoid_length ~ "state"
  )

  # Selects all rows of the full block-group-level fips code table that have a
  # number in common with user_geoids
  processed_geoids <- fips_table[which(fips_table[,1] %in% user_geoids |
                                       fips_table[,2] %in% user_geoids |
                                       fips_table[,3] %in% user_geoids |
                                       fips_table[,4] %in% user_geoids),]

  # Selects the column corresponding to the most specific level of geography
  # present in user_geoids, and eliminates duplicates
  processed_geoids <- unname(unique(processed_geoids[geography_level]))

  geoids <- list(processed_geoids, geography_level)
  class(geoids) <- "geoids"

  return(geoids)
}
