local({
  `%>%` <- magrittr::`%>%`
  
  ##############################################################################
  # State
  state_fips_key <-
    tidycensus::fips_codes %>% 
    dplyr::distinct(
      geoid = state_code,
      state_abbreviation = state,
      state_name
    )
  
  state_centers2010 <-
    readr::read_csv(
      "https://www2.census.gov/geo/docs/reference/cenpop2010/CenPop2010_Mean_ST.txt",
      col_types = 
        readr::cols(
          STATEFP = "c",
          STNAME = "_",
          POPULATION = "_",
          LATITUDE = "d",
          LONGITUDE = "d"
        )
    ) %>% 
    dplyr::select(
      geoid = STATEFP, 
      longitude = LONGITUDE,
      latitude = LATITUDE
    ) %>%
    dplyr::left_join(state_fips_key, by = "geoid")
  
  
  # ###########################################################################
  # County
  county_fips_key <-
    tidycensus::fips_codes %>% 
    dplyr::transmute(geoid = paste0(state_code, county_code), county)
  
  county_centers2010 <-
    readr::read_csv(
      "https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO.txt",
      col_types =
        readr::cols(
          STATEFP = "c",
          COUNTYFP = "c",
          COUNAME = "_",
          STNAME = "_",
          POPULATION = "_",
          LATITUDE = "d",
          LONGITUDE = "d"
        )
    ) %>% 
    dplyr::transmute(
      geoid = paste0(STATEFP, COUNTYFP),
      longitude = LONGITUDE,
      latitude = LATITUDE
    ) %>% 
    dplyr::left_join(county_fips_key, by = "geoid")
  
  # ###########################################################################
  # Tract
  tract_centers2010 <-
    readr::read_csv(
      "https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR.txt",
      col_types =
        readr::cols(
          STATEFP = "c",
          COUNTYFP = "c",
          TRACTCE = "c",
          POPULATION = "_",
          LATITUDE = "d",
          LONGITUDE = "d"
        )
    ) %>% 
    dplyr::transmute(
      geoid = paste0(STATEFP, COUNTYFP, TRACTCE),
      longitude = LONGITUDE,
      latitude = LATITUDE
    )
  
  # ###########################################################################
  # Block group
  block_group_centers2010 <-
    readr::read_csv(
      "https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt",
      col_types =
        readr::cols(
          STATEFP = "c",
          COUNTYFP = "c",
          TRACTCE = "c",
          BLKGRPCE = "c",
          POPULATION = "_",
          LATITUDE = "d",
          LONGITUDE = "d"
        )
    ) %>% 
    dplyr::transmute(
      geoid = paste0(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE),
      longitude = LONGITUDE,
      latitude = LATITUDE
    )
  
  usethis::use_data(
    state_centers2010, county_centers2010, tract_centers2010,
    block_group_centers2010, 
    overwrite = TRUE,
    compress = "xz"
  )
})