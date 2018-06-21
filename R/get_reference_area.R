get_reference_area <- function(geography=NULL, ...) {
  dots <- eval(substitute(alist(...)))
  
  # if(length(dots) == 0) {
  #   ref_area <- "us"
  # }
  
  # Throws an error if user included more than one variable identifying the
  # reference area
  
  if(length(dots) > 1 & all(names(dots) %in% c("GEOIDs", "states"))) {
    stop("Reference area may only include one of: GEOIDs, states.")
  } 
  
  if("GEOIDs" %in% names(dots)) {
    ref_area <- eval(dots[["GEOIDs"]])
  } else if("states" %in% names(dots)) {
    ref_area <- eval(dots[["states"]])
  } else {
    message("No valid reference area given. Using entire US as reference area.")
    ref_area <- "us"
  }
  
  if(geography == "state") {
    ref_area <- unique(unlist(purrr::map(ref_area,
                                         tidycensus:::validate_state)))
  }

  ref_area <- dplyr::case_when(
    geography == "state" ~ tidycensus:::validate_state())
  ref_area <- parse_GEOIDs(dots[["GEOIDs"]])
  
  ref_area <-
    acs::fips.state$STATE[acs::fips.state$STUSAB %in% eval(dots[["states"]])]
  ref_area <- stringr::str_pad(as.character(ref_area),
                               width = 2, side = "left", pad = "0")
  
  return(ref_area)

}
