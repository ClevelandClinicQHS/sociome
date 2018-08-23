# This function is automatically run when the sociome package is loaded (see
# ?.onLoad).
#
# When the sociome package is loaded, the internal object called
# us_block_groups, which is a numeric vector of all block groups in the 50
# states plus the District of Columbia (DC) and Puerto Rico (PR), is parsed and
# turned into a data frame. This data frame is then given the name fips_table,
# which is present in the exported namespace of the sociome package.
#
# In other words, the exported data set called fips_table is not actually
# present in the sociome package when downloaded; rather, it is built when
# sociome is loaded. This serves to save space.
.onLoad <- function(libname, pkgname) {
  
  us_block_groups <- stringr::str_pad(us_block_groups, width = 12,
                                      side = "left", pad = "0")
  
  fips_table <-
    data.frame(`block group` = us_block_groups,
               tract = stringr::str_sub(us_block_groups, 1, 11),
               county = stringr::str_sub(us_block_groups, 1, 5),
               state = stringr::str_sub(us_block_groups, 1, 2),
               short_county = stringr::str_sub(us_block_groups, 3, 5),
               
               check.names = FALSE,
               stringsAsFactors = FALSE)
  
  assign("fips_table", fips_table, pos = asNamespace("sociome"))
}