
parse_states <- function(states) {

  if(!is.vector(states)) {
    stop("states must be a character vector")
  }

  if(!is.character(states)) {
    stop("states must be a character vector")
  }

  if(!all(stringr::str_detect(states, "^[[:digit:]]{1,}$"))) {
    stop("states must consist of digits only (i.e., 0-9)")
  }

  GEOID_length <- unique(nchar(states))

  # Checks to see if all elements of domain have the same number of characters
  if(length(GEOID_length) > 1) {
    stop("All states must have the same number of digits")
  }

  if(!(GEOID_length %in% c(2, 5, 11, 12))) {
    stop("Incompatible GEOID length. See help('parse_states') for details")
  }

  geography <- dplyr::case_when(
    GEOID_length == 2  ~ "state",
    GEOID_length == 5  ~ "county",
    GEOID_length == 11 ~ "tract",
    GEOID_length == 12 ~ "block group"
  )

  states <- list(geography, states)
  class(states) <- "states"

  return(states)
}
