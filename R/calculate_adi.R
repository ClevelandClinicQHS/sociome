calculate_adi <- function(ref_area, get_acs_args) {

  # Saves old tigris_use_cache value and puts it back when function exits
  old <- options(tigris_use_cache = TRUE)
  on.exit(options(old), add = TRUE)
  
  # purrr::map() is used to call tidycensus::get_acs() for each user-specified state
  # or set of user-specified states.
  # purrr::reduce(rbind) puts the results into a single data frame
  acs_data_raw <-
    ref_area$state_county %>% 
    purrr::map(
      function(state_county, get_acs_args) {
        state  <- state_county$state
        county <- state_county$county
        do.call(eval(parse(text = "tidycensus::get_acs")),
                c(list(state = state, county = county), get_acs_args))
        },
      get_acs_args = get_acs_args) %>%
    purrr::reduce(rbind)
  
  # Since the call (or calls) to tidycensus::get_acs() above usually gathers
  # data on more places than what the user specified, this pares the data frame
  # down to only include the user-specified reference area.
  acs_ref_area <- acs_data_raw %>%
    dplyr::filter(GEOID %in% ref_area$ref_geoids)
  
  # Selects the relevant variables that tidycensus::get_acs() wrangles them into
  # a data frame that contains the specific measures that are used to calculate
  # ADI
  acs_data_f <- acs_ref_area %>%
    as.data.frame() %>% # This causes the geometry column to become un-"sticky",
                        # allowing it to be removed so that it doesn't interfere
                        # with the imputation that may follow.
    dplyr::select(B01003_001E, B19013_001E, B19001_002E,
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
    dplyr::select(B19013_001E, B25088_001E, B25064_001E,
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
  
  # Performs single imputation if there is any missingness in the data.
  if(anyNA(acs_data_f)) {
    
    is.na(acs_data_f) <- do.call(cbind, lapply(acs_data_f, is.infinite))

    acs_data_f <- acs_data_f %>% 
      mice::mice(m = 1, maxit = 50, method = "pmm", seed = 500,
                 printFlag = FALSE) %>% 
      mice::complete()
    
    message("Single imputation performed")
  }
  
  # Where the magic happens: a factor analysis of the statistics that produces
  # the raw ADI scores
  fit <- psych::fa(acs_data_f, nfactors = 1, rotate = "none", fm = "pa",
                   max.iter = 25)
  
  # The raw ADI scores are standardized to have a mean of 100 and sd of 20
  acs_adi <- acs_ref_area %>% 
    dplyr::mutate(ADI = as.numeric(fit$scores*20+100)) %>% 
    dplyr::select(GEOID, NAME, ADI)

  return(acs_adi)
}

#' Import mice.impute.pmm from mice package
#' 
#' This simply imports mice.impute.pmm from the mice package.
#' 
#' This is only here so that mice::mice() will work. It needs mice.impute.pmm(),
#' and it will not find it unless it is included as an exported function in
#' sociome.
#'
#' @export
mice.impute.pmm <- mice::mice.impute.pmm