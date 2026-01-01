## code to prepare `acs_age_sex_race_ethnicity_vars` dataset goes here

library(here)
library(tidycensus)
library(tidyverse)

acs_age_sex_race_ethnicity_vars <-
  bind_rows(
    tidycensus::load_variables(year = 2020, dataset = "acs5", cache = TRUE) %>%
      filter(str_detect(name, "^B01001_")) %>%
      transmute(
        variable = name,
        description = str_replace(label, "^Estimate!!Total:!!", ""),
        description = str_replace(description, "^(\\w+ale):", "\\1s"),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = c("total", description[-1]),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2020, dataset = "acs5", cache = TRUE) %>%
      filter(
        str_detect(name, "^B03002_"),
        str_detect(label, ":!!Two or more races:!!", negate = TRUE)
      ) %>%
      slice(-1) %>%
      transmute(
        variable = name,
        description = str_replace(label, "^Estimate!!Total:!!", ""),
        description = str_replace_all(description, ":", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      )
  )

usethis::use_data(acs_age_sex_race_ethnicity_vars, overwrite = TRUE)
tools::resaveRdaFiles(
  here("data", "acs_age_sex_race_ethnicity_vars.rda"),
  compress = "auto"
)
