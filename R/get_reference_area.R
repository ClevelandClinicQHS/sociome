get_reference_area <- function(geography=NULL, ...) {
  dots <- eval(substitute(alist(...)))

  # Throws an error if user included more than one variable identifying the
  # reference area

  if(length(dots) > 1 & all(names(dots) %in% c("GEOIDs", "states"))) {
    stop("Reference area may only include one of: GEOIDs, states.")
  }

  if("GEOIDs" %in% names(dots)) {
    ref_area <- parse_GEOIDs(dots[["GEOIDs"]])
  }
  else if("states" %in% names(dots)) {
    ref_area <-
      acs::fips.state$STATE[acs::fips.state$STUSAB %in% eval(dots[["states"]])]
    ref_area <- stringr::str_pad(as.character(ref_area),
                                 width = 2, side = "left", pad = "0")
  }

  return(ref_area)

}
