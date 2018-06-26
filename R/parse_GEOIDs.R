

#' parse_GEOIDs
#'
#' Scans a vector of GEOIDs for problems, and returns a list of class "GEOIDs"
#' to be used by get_reference_area()
#'
#' @param GEOIDs a character vector of GEOIDs. All elements of the vector must
#'   contain the same number of digits so that they represent the same level of
#'   geography.
#'
#' @return A list of class "GEOIDs", which contains the original vector of
#'   GEOIDs and a string indicating the level of geography that the
#'   GEOIDs represent (one of "state", "county", "tract", or "block group").
# @export

parse_GEOIDs <- function(GEOIDs) {

  # Trim whitespace
  GEOIDs <- stringr::str_trim(GEOIDs)

  # GEOIDs must be a vector of strings of digits between 2 and 12 characters
  checkmate::assert_character(
    GEOIDs, pattern = "^[[:digit:]]{2,12}$", min.len = 1, any.missing = FALSE)

  GEOID_length <- unique(nchar(GEOIDs))

  browser()

  # Checks to see if all elements of domain have the same number of characters
  # if(length(GEOID_length) > 1) {
  #   stop("All GEOIDs must have the same number of digits")
  # }

  if(!all(GEOID_length %in% c(2, 5, 11, 12))) {
    stop("All GEOIDs must contain either 2 digits (states), 5 digits (counties), 11 digits (census tracts), or 12 digits (census block groups)")
  }

  geography_level <- dplyr::case_when(
    12 %in% GEOID_length ~ "block group",
    11 %in% GEOID_length ~ "tract",
     5 %in% GEOID_length ~ "county",
     2 %in% GEOID_length ~ "state"
  )

  GEOIDs <- list(geography_level, GEOIDs)
  class(GEOIDs) <- "GEOIDs"

  return(GEOIDs)

}
