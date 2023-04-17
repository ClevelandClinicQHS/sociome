nmcounties2017 <-
  sociome::get_adi(
    geography = "county",
    state = "NM",
    keep_indicators = FALSE,
    geometry = FALSE,
    year = 2017
  )

nmcounties2010 <-
  sociome::get_adi(
    geography = "county",
    state = "NM",
    keep_indicators = FALSE,
    geometry = FALSE,
    year = 2010,
    dataset = "decennial",
    show_call = TRUE
  )

nmcounties2000 <-
  sociome::get_adi(
    geography = "county",
    state = "NM",
    keep_indicators = FALSE,
    geometry = FALSE,
    year = 2000,
    dataset = "decennial"
  )

# Not run because 1990 data is currently not accessible via Census API.
# This will be restored if the Census API ever fixes it.
# nmcounties1990 <-
#   get_adi(
#     geography = "county",
#     state = "NM",
#     keep_indicators = TRUE,
#     geometry = FALSE,
#     year = 1990,
#     raw_data_only = TRUE,
#     dataset = "decennial"
#   )

usethis::use_data(
  nmcounties2017, nmcounties2010, nmcounties2000, # nmcounties1990,
  internal = TRUE,
  overwrite = TRUE,
  compress = "xz"
)
