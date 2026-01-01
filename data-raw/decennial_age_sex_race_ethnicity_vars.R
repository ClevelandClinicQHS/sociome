
library(here)
library(tidyverse)

decennial_age_sex_race_ethnicity_vars <-
  bind_rows(
    tidycensus::load_variables(year = 2000, dataset = "sf1", cache = TRUE) %>%
      filter(str_detect(name, "^P012\\d{3}$")) %>%
      transmute(
        year = 2000L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, "^(\\w+ale)", "\\1s"),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2000, dataset = "sf1", cache = TRUE) %>%
      filter(str_detect(name, "^P008\\d{3}$")) %>%
      slice(-1) %>%
      transmute(
        year = 2000L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2010, dataset = "sf1", cache = TRUE) %>%
      filter(str_detect(name, "^P012\\d{3}$")) %>%
      transmute(
        year = 2010L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, "^(\\w+ale)", "\\1s"),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2010, dataset = "sf1", cache = TRUE) %>%
      filter(str_detect(name, "^P005\\d{3}$")) %>%
      slice(-1) %>%
      transmute(
        year = 2010L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2020, dataset = "dhc", cache = TRUE) %>%
      filter(str_detect(name, "^P12_")) %>%
      transmute(
        year = 2020L,
        variable = name,
        description =
          str_replace_all(
            label,
            c("^ !!(Total:!!)?" = "",
              ":$" = "",
              "^(\\w+ale)" = "\\1s",
              " years" = "",
              "[ :!]+" = "_")
          ),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2020, dataset = "dhc", cache = TRUE) %>%
      filter(str_detect(name, "^P5_")) %>%
      slice(-1) %>%
      transmute(
        year = 2020L,
        variable = name,
        description =
          str_replace_all(
            label,
            c("^ !!Total:!!" = "", ":!!| " = "_", ":$" = "")
          ),
        # description = str_replace(description, " years", ""),
        # description = str_replace_all(description, ":!!| ", "_"),
        description = tolower(description)
      )
  )

usethis::use_data(
  decennial_age_sex_race_ethnicity_vars,
  overwrite = TRUE
)
tools::resaveRdaFiles(
  here("data", "decennial_age_sex_race_ethnicity_vars.rda"),
  compress = "auto"
)
