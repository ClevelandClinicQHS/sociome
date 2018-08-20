#' @import mice
#' @importFrom rlang .data
calculate_adi <- function(ref_area, get_acs_args) {

  # Saves old tigris_use_cache value and puts it back when function exits
  old <- options(tigris_use_cache = TRUE)
  on.exit(options(old), add = TRUE)
  
  # purrr::map() is used to call tidycensus::get_acs() for each user-specified
  # state or set of user-specified states.
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
    dplyr::filter(.data$GEOID %in% ref_area$ref_geoids)
  
  # Selects the relevant variables that tidycensus::get_acs() wrangles them into
  # a data frame that contains the specific measures that are used to calculate
  # ADI
  acs_data_f <- acs_ref_area %>%
    as.data.frame() %>% # This causes the geometry column to become un-"sticky",
                        # allowing it to be removed so that it doesn't interfere
                        # with the imputation that may follow.
    dplyr::select(.data$B01003_001E, .data$B19013_001E, .data$B19001_002E,
                  .data$B19001_011E, .data$B19001_012E, .data$B19001_013E,
                  .data$B19001_014E, .data$B19001_015E, .data$B19001_016E,
                  .data$B19001_017E, .data$B17010_001E, .data$B17010_002E, 
                  .data$B25003_001E, .data$B25003_002E, .data$C17002_001E,
                  .data$C17002_002E, .data$C17002_003E, .data$C17002_004E, 
                  .data$C17002_005E, .data$B25044_001E, .data$B25044_003E, 
                  .data$B25044_010E, .data$B25014_001E, .data$B25014_005E,
                  .data$B25014_006E, .data$B25014_007E, .data$B25014_011E,
                  .data$B25014_012E, .data$B25014_013E, .data$B25088_001E,
                  .data$B25064_001E, .data$B25077_001E, .data$C24010_001E,
                  .data$C24010_003E, .data$C24010_039E, .data$B23025_001E,
                  .data$B23025_005E, .data$B15003_001E, .data$B15003_002E,
                  .data$B15003_003E, .data$B15003_004E, .data$B15003_005E,
                  .data$B15003_006E, .data$B15003_007E, .data$B15003_008E,
                  .data$B15003_009E, .data$B15003_010E, .data$B15003_011E,
                  .data$B15003_012E, .data$B15003_017E, .data$B15003_018E,
                  .data$B15003_019E, .data$B15003_020E, .data$B15003_021E,
                  .data$B15003_022E, .data$B15003_023E, .data$B15003_024E,
                  .data$B15003_025E, .data$B23008_001E, .data$B23008_008E,
                  .data$B23008_021E) %>%
    dplyr::mutate(Fpoverty        = .data$B17010_002E / .data$B17010_001E,
                  OwnerOcc        = .data$B25003_002E / .data$B25003_001E,
                  incomegreater50 = .data$B19001_011E + .data$B19001_012E +
                    .data$B19001_013E + .data$B19001_014E + .data$B19001_015E +
                    .data$B19001_016E + .data$B19001_017E,
                  IncomeDisparity = log(100 * (.data$B19001_002E / 
                                                 .data$incomegreater50)),
                  less150poverty  = .data$C17002_002E + .data$C17002_003E +
                    .data$C17002_004E + .data$C17002_005E,
                  less150FPL      = .data$less150poverty / .data$C17002_001E,
                  oneparent       = .data$B23008_008E + .data$B23008_021E,
                  singlePHH       = .data$oneparent / .data$B23008_001E,
                  vehiclesum      = .data$B25044_003E + .data$B25044_010E,
                  pnovehicle      = .data$vehiclesum / .data$B25044_001E,
                  sumprofs        = .data$C24010_003E + .data$C24010_039E,
                  whitecollar     = .data$sumprofs / .data$C24010_001E,
                  unemployed      = .data$B23025_005E / .data$B23025_001E,
                  Nhighschoolup   = .data$B15003_017E + .data$B15003_018E +
                    .data$B15003_019E + .data$B15003_020E + .data$B15003_021E +
                    .data$B15003_022E + .data$B15003_023E + .data$B15003_024E +
                    .data$B15003_025E,
                  Phighschoolup   = .data$Nhighschoolup / .data$B15003_001E,
                  Nless9thgrade   = .data$B15003_002E + .data$B15003_003E +
                    .data$B15003_004E + .data$B15003_005E + .data$B15003_006E +
                    .data$B15003_007E + .data$B15003_008E + .data$B15003_009E +
                    .data$B15003_010E + .data$B15003_011E + .data$B15003_012E,
                  Pless9grade     = .data$Nless9thgrade / .data$B15003_001E,
                  SUMcrowded      = .data$B25014_005E + .data$B25014_006E +
                    .data$B25014_007E + .data$B25014_011E + .data$B25014_012E +
                    .data$B25014_013E,
                  Ocrowded        = .data$SUMcrowded / .data$B25014_001E) %>%
    dplyr::select(.data$B19013_001E, .data$B25088_001E, .data$B25064_001E,
                  .data$B25077_001E, .data$Fpoverty, .data$OwnerOcc,
                  .data$IncomeDisparity, .data$less150FPL, .data$singlePHH,
                  .data$pnovehicle, .data$whitecollar, .data$unemployed,
                  .data$Phighschoolup, .data$Pless9grade, .data$Ocrowded) %>%
    dplyr::rename(
      "medianHouseholdIncome" = .data$B19013_001E,
      "medianMortgage" = .data$B25088_001E,
      "medianRent" = .data$B25064_001E,
      "medianHouseValue" = .data$B25077_001E ,
      "pctFamiliesInPoverty" = .data$Fpoverty,
      "pctOwnerOccupiedHousing" = .data$OwnerOcc,
      "ratioThoseMakingUnder10kToThoseMakingOver50k" = .data$IncomeDisparity,
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = .data$less150FPL,
      "pctChildrenInSingleParentHouseholds" = .data$singlePHH ,
      "pctHouseholdsWithNoVehicle" = .data$pnovehicle,
      "pctPeopleWithWhiteCollarJobs"  = .data$whitecollar,
      "pctPeopleUnemployed" = .data$unemployed,
      "pctPeopleWithAtLeastHSEducation"= .data$Phighschoolup,
      "pctPeopleWithLessThan9thGradeEducation" = .data$Pless9grade,
      "pctHouseholdsWithOverOnePersonPerRoom" =  .data$Ocrowded)
  
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
    dplyr::mutate(ADI = as.numeric(fit$scores * 20 + 100)) %>% 
    dplyr::select(.data$GEOID, .data$NAME, .data$ADI)

  return(acs_adi)
}