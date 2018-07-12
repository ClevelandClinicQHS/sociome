calculate_adi <- function(ref_area, year, survey, key) {
  
  # acs_data_raw <-
  #   purrr::map_dfr(ref_area$state_county, call_get_acs,
  #                  geography = ref_area$geography,
  #                  year = year, survey = survey, key = key)
  
  acs_data_raw <-
    purrr::map_dfr(
      ref_area$state_county,
      function(state_county, geography, year, survey, key) {
        state <- state_county$state
        county <- state_county$county
        tidycensus::get_acs(
          geography = geography, state = state, county = county,
          year = year, survey = survey, key = key, cache_table = TRUE,
          variables =
            c("B01003_001","B19013_001","B19001_002","B19001_011","B19001_012",
              "B19001_013","B19001_014","B19001_015","B19001_016","B19001_017",
              "B17010_001","B17010_002","B25003_001","B25003_002","C17002_001",
              "C17002_002","C17002_003","C17002_004","C17002_005","B25044_001",
              "B25044_003","B25044_010","B25014_001","B25014_005","B25014_006",
              "B25014_007","B25014_011","B25014_012","B25014_013","B25088_001",
              "B25064_001","B25077_001","C24010_001","C24010_003","C24010_039",
              "B23025_001","B23025_005","B15003_001","B15003_002","B15003_003",
              "B15003_004","B15003_005","B15003_006","B15003_007","B15003_008",
              "B15003_009","B15003_010","B15003_011","B15003_012","B15003_017",
              "B15003_018","B15003_019","B15003_020","B15003_021","B15003_022",
              "B15003_023","B15003_024","B15003_025","B23008_001","B23008_008",
              "B23008_021"),
          output = "wide", table = NULL, geometry = FALSE, shift_geo = FALSE, 
          keep_geo_vars = FALSE, summary_var = NULL, moe_level = 90)
        },
      geography = ref_area$geography, year = year, survey = survey, key = key)

  acs_data <- acs_data_raw %>%
    dplyr::filter(GEOID %in% ref_area$ref_geoids) %>%
    dplyr::select(GEOID, NAME, B01003_001E, B19013_001E, B19001_002E,
                  B19001_011E, B19001_012E, B19001_013E, B19001_014E,
                  B19001_015E, B19001_016E, B19001_017E, B17010_001E,
                  B17010_002E, B25003_001E, B25003_002E, C17002_001E,
                  C17002_002E, C17002_003E, C17002_004E, C17002_005E,
                  B25044_001E, B25044_003E, B25044_010E, B25014_001E,
                  B25014_005E, B25014_006E, B25014_007E, B25014_011E,
                  B25014_012E, B25014_013E, B25088_001E, B25064_001E,
                  B25077_001E, C24010_001E, C24010_003E, C24010_039E,
                  B23025_001E, B23025_005E, B15003_001E, B15003_002E, 
                  B15003_003E, B15003_004E, B15003_005E, B15003_006E,
                  B15003_007E, B15003_008E, B15003_009E, B15003_010E,
                  B15003_011E, B15003_012E, B15003_017E, B15003_018E,
                  B15003_019E, B15003_020E, B15003_021E, B15003_022E,
                  B15003_023E, B15003_024E, B15003_025E, B23008_001E,
                  B23008_008E, B23008_021E) %>%
    dplyr::mutate(Fpoverty        = B17010_002E / B17010_001E,
                  OwnerOcc        = B25003_002E / B25003_001E,
                  incomegreater50 =
                    B19001_011E + B19001_012E + B19001_013E + B19001_014E +
                    B19001_015E + B19001_016E + B19001_017E,
                  IncomeDisparity = log(100*(B19001_002E / incomegreater50)),
                  less150poverty  =
                    C17002_002E + C17002_003E + C17002_004E + C17002_005E,
                  less150FPL      = less150poverty / C17002_001E,
                  oneparent       = B23008_008E + B23008_021E,
                  singlePHH       = oneparent / B23008_001E,
                  vehiclesum      = B25044_003E + B25044_010E,
                  pnovehicle      = vehiclesum / B25044_001E,
                  sumprofs        = C24010_003E + C24010_039E,
                  whitecollar     = sumprofs / C24010_001E,
                  unemployed      = B23025_005E / B23025_001E,
                  Nhighschoolup   =
                    B15003_017E + B15003_018E + B15003_019E + B15003_020E +
                    B15003_021E + B15003_022E + B15003_023E + B15003_024E +
                    B15003_025E,
                  Phighschoolup   = Nhighschoolup / B15003_001E,
                  Nless9thgrade   =
                    B15003_002E + B15003_003E + B15003_004E + B15003_005E +
                    B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                    B15003_010E + B15003_011E + B15003_012E,
                  Pless9grade     = Nless9thgrade / B15003_001E,
                  SUMcrowded      =
                    B25014_005E + B25014_006E + B25014_007E + B25014_011E +
                    B25014_012E + B25014_013E,
                  Ocrowded        = SUMcrowded / B25014_001E) %>%
    dplyr::select(GEOID, NAME, B19013_001E, B25088_001E, B25064_001E,
                  B25077_001E, Fpoverty, OwnerOcc, IncomeDisparity, less150FPL,
                  singlePHH, pnovehicle, whitecollar, unemployed, Phighschoolup,
                  Pless9grade, Ocrowded) %>%
    dplyr::rename(
      "medianHouseholdIncome" = B19013_001E,
      "medianMortgage" = B25088_001E,
      "medianRent" = B25064_001E,
      "medianHouseValue" = B25077_001E ,
      "pctFamiliesInPoverty" = Fpoverty,
      "pctOwnerOccupiedHousing" = OwnerOcc,
      "ratioThoseMakingUnder10kToThoseMakingOver50k" = IncomeDisparity,
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = less150FPL,
      "pctChildrenInSingleParentHouseholds" = singlePHH ,
      "pctHouseholdsWithNoVehicle" = pnovehicle,
      "pctPeopleWithWhiteCollarJobs"  = whitecollar,
      "pctPeopleUnemployed" = unemployed,
      "pctPeopleWithAtLeastHSEducation"= Phighschoolup,
      "pctPeopleWithLessThan9thGradeEducation" = Pless9grade,
      "pctHouseholdsWithOverOnePersonPerRoom" =  Ocrowded)

  acs_data_f <- acs_data %>%
    dplyr::select(-c("GEOID", "NAME"))
  
  if(anyNA(acs_data_f)) {
    # Performs multiple imputation if there is any missingness in the data.

    is.na(acs_data_f) <- do.call(cbind, lapply(acs_data_f, is.infinite))

    tempdf <- mice::mice(acs_data_f, m = 5, maxit = 50, method = "pmm",
                         seed = 500, printFlag = FALSE)
    
    acs_data_f <- mice::complete(tempdf, 1)
    
    message("Multiple imputation performed")
  }
  
  # factor analysis
  fit <- psych::fa(acs_data_f, nfactors = 1, rotate = "none", fm = "pa",
                   max.iter = 25)
  acs_data$ADI <- as.numeric(fit$scores*20+100)

  acs_adi <- acs_data %>%
    dplyr::select(GEOID, NAME, ADI)

  return(acs_adi)
}

# call_get_acs <- function(state_county, geography, year, survey, key) {
#   state <- state_county$state
#   county <- state_county$county
#   tidycensus::get_acs(
#     geography = geography,
#     state = state, county = county,
#     year = year, survey = survey, key = key,
#     variables =
#       c("B01003_001","B19013_001","B19001_002","B19001_011",
#         "B19001_012","B19001_013","B19001_014","B19001_015",
#         "B19001_016","B19001_017","B17010_001","B17010_002",
#         "B25003_001","B25003_002","C17002_001","C17002_002",
#         "C17002_003","C17002_004","C17002_005","B25044_001",
#         "B25044_003","B25044_010","B25014_001","B25014_005",
#         "B25014_006","B25014_007","B25014_011","B25014_012",
#         "B25014_013","B25088_001","B25064_001","B25077_001",
#         "C24010_001","C24010_003","C24010_039","B23025_001",
#         "B23025_005","B15003_001","B15003_002","B15003_003",
#         "B15003_004","B15003_005","B15003_006","B15003_007",
#         "B15003_008","B15003_009","B15003_010","B15003_011",
#         "B15003_012","B15003_017","B15003_018","B15003_019",
#         "B15003_020","B15003_021","B15003_022","B15003_023",
#         "B15003_024","B15003_025","B23008_001","B23008_008",
#         "B23008_021"),
#     output = "wide", table = NULL, cache_table = TRUE,
#     geometry = FALSE, keep_geo_vars = FALSE,
#     shift_geo = FALSE, summary_var = NULL, moe_level = 90)
# }