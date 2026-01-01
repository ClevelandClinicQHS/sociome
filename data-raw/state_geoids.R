## code to prepare `state_geoids` dataset goes here

library(here)
library(tidyverse)

state_geoids <-
  tidycensus::fips_codes |>
  distinct(state_name, state_code) |>
  filter(
    !match(
      state_name,
      c("American Samoa", "Guam", "Northern Mariana Islands",
        "U.S. Minor Outlying Islands", "U.S. Virgin Islands"),
      0
    )
  ) |>
  deframe()

usethis::use_data(state_geoids, overwrite = TRUE)
tools::resaveRdaFiles(
  here("data", "state_geoids.rda"),
  compress = "auto"
)
