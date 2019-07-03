#' Calculate ADI from census data.
#'
#' Calculate the area deprivation index using decennial Census or American
#' Community Survey (ACS) variables.
#'
#' This function discerns between ACS, year 2000 decennial census, and year 1990
#' decennial census data by checking for the existence of the population
#' variables of each (\code{B01003_001}, \code{P001001}, and \code{P0010001},
#' respectively).
#'
#' @param data Raw data obtained via
#'   \code{tidycensus::\link[tidycensus]{get_acs}(output = "wide")} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}(output = "wide")},
#'   having the variables necessary to compute the indicators of the ADI, which
#'   can be found in \code{sociome::\link{acs_vars}},
#'   \code{sociome::decennial_vars_1990}, or
#'   \code{sociome::decennial_vars_2000}.
#' @param keep_indicators Logical indicating whether or not to keep the
#'   component indicators of the ADI as well as the original census variables
#'   used to calculate them. Defaults to \code{FALSE}.
#'
#' @importFrom rlang .data
#' @export
calculate_adi <- function(data, keep_indicators = FALSE) {
  
  if (!is.data.frame(data)) {
    stop("data must be a tibble, sf tibble, or data-frame-like object")
  }
  
  if (nrow(data) < 30L) {
    warning(
      "\nCalculating ADI values from fewer than 30 locations.\nIt is ",
      "recommended to add more in order to obtain trustworthy results."
    )
  }
  
  data_f <- calculate_indicators(data)
  
  # Performs single imputation if there is any missingness in the data.
  if (anyNA(data_f)) {
    
    is.na(data_f) <- is.infinite(as.matrix.data.frame(data_f))
    
    impute_attempt <- 
      try(
        data_f %>%
          mice::mice(
            m = 1L, 
            maxit = 50L, 
            method = "pmm", 
            seed = 500L,
            printFlag = FALSE
          ) %>%
          mice::complete()
      )
    
    if (class(impute_attempt) == "try-error") {
      message(
        "\nImputation unsuccessful.\nReturning factors that could not ",
        "be imputed followed by raw census data."
      )
      return(
        data %>% 
          as.data.frame() %>% 
          dplyr::bind_cols(data_f, .) %>% 
          dplyr::select(
            "GEOID",
            dplyr::starts_with("NAME"),
            dplyr::everything()
          ) %>% 
          tibble::as_tibble()
      )
    }
    
    data_f <- impute_attempt
    
    message("\nSingle imputation performed")
  }
  
  if (keep_indicators) {
    keep_columns <- append(names(data), c("ADI", names(data_f)), after = 2L)
    data         <- dplyr::bind_cols(data, data_f)
  } else {
    keep_columns <- alist("GEOID", dplyr::starts_with("NAME"), "ADI")
  }
  
  # Where the magic happens: a principal-components analysis (PCA) of the
  # statistics that produces the raw ADI scores
  fit <- psych::principal(data_f)
  
  # Sometimes the PCA produces results that are completely reversed (i.e., it
  # gives deprived areas low ADIs and less deprived areas high ADIs). Therefore,
  # this function performs a check to see if this has occured.
  #   1. The signage of the factor loadings are multiplied by their expected
  #      signage according to Singh's original research (present in the unnamed
  #      vector of 1s and -1s below). This produces a vector of 1s and -1s, with
  #      a 1 indicating a factor loading in the expected direction and a -1
  #      indicating a factor loading in the wrong direction.
  #   2. The sum() of this vector is computed.
  #   3. The sign() of this sum is computed and saved into a variable called
  #      "signage_flipper". It will equal 1 or -1. It will equal 1 if most of
  #      the factor loadings have the same sign as the original Singh factor
  #      loadings. It will be -1 if not. It will never equal 0 because there is
  #      an odd number of factor loadings.
  signage_flipper <-
    sign(
      sum(
        sign(fit$loadings) *
          c(-1L, -1L, -1L, -1L, 1L, -1L, 1L, 1L, 1L, 1L, -1L, 1L, -1L, 1L, 1L)
      )
    )
  #   4. The variable signage_flipper is multiplied by the PCA scores before
  #      standardization. In effect, this flips the ADIs in the right direction
  #      (multiplies their scores by -1) if they were reversed, and it keeps
  #      them the same (multiplies their scores by 1) if they were not reversed.
  # The raw ADI scores are standardized to have a mean of 100 and sd of 20
  adi <- data %>%
    dplyr::mutate(
      ADI = as.numeric(fit$scores * signage_flipper * 20 + 100)
    ) %>%
    dplyr::select(!!!keep_columns)
  
  if (!inherits(adi, "sf")) {
    adi <- tibble::as_tibble(adi)
  }
  
  attr(adi, "loadings") <-
    stats::setNames(
      object = as.vector(fit$loadings),
      nm     = row.names(fit$loadings)
    )
  
  class(adi) <- c(class(adi), "adi")
  
  adi
}



calculate_indicators <- function(data) {
  if (!is.null(data$B01003_001E)) {
    factors_from_acs(data)
  } else if (!is.null(data$P001001)) {
    factors_from_2000_decennial(data)
  } else if (!is.null(data$P0010001)) {
    factors_from_1990_decennial(data)
  } else {
    stop(
      "Data not recognized as having the variables necessary to calculate ADI."
    )
  }
}



# Selects the relevant variables from the tidycensus::get_acs() output, then
# wrangles them into a data frame that contains the specific measures that are
# used to calculate ADI
#' @importFrom rlang .data
factors_from_acs <- function(data) {
  
  data <- as.data.frame(data)
  # In case data is an sf tibble, this causes the geometry column to become
  # "unsticky", allowing it to be removed by the subsequent dplyr::select()
  # command so that it doesn't interfere with the imputation that may follow.
  
  if (is.null(data$B23025_005E)) {
    data <- data %>% 
      dplyr::mutate(
        B23025_005E = .data$B23001_008E + .data$B23001_015E +
          .data$B23001_022E + .data$B23001_029E + .data$B23001_036E +
          .data$B23001_043E + .data$B23001_050E + .data$B23001_057E +
          .data$B23001_064E + .data$B23001_071E + .data$B23001_076E +
          .data$B23001_081E + .data$B23001_086E + .data$B23001_094E +
          .data$B23001_101E + .data$B23001_108E + .data$B23001_115E +
          .data$B23001_122E + .data$B23001_129E + .data$B23001_136E +
          .data$B23001_143E + .data$B23001_150E + .data$B23001_157E +
          .data$B23001_162E + .data$B23001_167E + .data$B23001_172E,
        B23025_003E = .data$B23001_006E + .data$B23001_013E +
          .data$B23001_020E + .data$B23001_027E + .data$B23001_034E +
          .data$B23001_041E + .data$B23001_048E + .data$B23001_055E +
          .data$B23001_062E + .data$B23001_069E + .data$B23001_074E +
          .data$B23001_079E + .data$B23001_084E + .data$B23001_092E +
          .data$B23001_099E + .data$B23001_106E + .data$B23001_113E +
          .data$B23001_120E + .data$B23001_127E + .data$B23001_134E +
          .data$B23001_141E + .data$B23001_148E + .data$B23001_155E +
          .data$B23001_160E + .data$B23001_165E + .data$B23001_170E
      )
  }
  
  if (is.null(data$B15002_001E)) {
    data <- data %>% 
      dplyr::mutate(
        Nless9thgrade = .data$B15003_002E +  .data$B15003_003E + 
          .data$B15003_004E + .data$B15003_005E + .data$B15003_006E + 
          .data$B15003_007E + .data$B15003_008E + .data$B15003_009E + 
          .data$B15003_010E + .data$B15003_011E + .data$B15003_012E,
        Nhighschoolup = .data$B15003_017E + .data$B15003_018E +
          .data$B15003_019E + .data$B15003_020E + .data$B15003_021E +
          .data$B15003_022E + .data$B15003_023E + .data$B15003_024E +
          .data$B15003_025E
      )
  } else {
    data <- data %>% 
      dplyr::mutate(
        Nless9thgrade = .data$B15002_003E +  .data$B15002_020E +
          .data$B15002_004E +  .data$B15002_021E +  .data$B15002_005E + 
          .data$B15002_022E +  .data$B15002_006E +  .data$B15002_023E,
        Nhighschoolup = .data$B15002_011E + .data$B15002_028E +
          .data$B15002_012E +  .data$B15002_029E + .data$B15002_013E +
          .data$B15002_030E + .data$B15002_014E + .data$B15002_031E +
          .data$B15002_015E + .data$B15002_032E + .data$B15002_016E +
          .data$B15002_033E + .data$B15002_017E + .data$B15002_034E +
          .data$B15002_018E + .data$B15002_035E,
        B15003_001E   = .data$B15002_001E
      )
  }
  
  data <- data %>% 
    dplyr::mutate(
      Fpoverty        = .data$B17010_002E / .data$B17010_001E,
      OwnerOcc        = .data$B25003_002E / .data$B25003_001E,
      incomegreater50 = .data$B19001_011E + .data$B19001_012E +
        .data$B19001_013E + .data$B19001_014E + .data$B19001_015E +
        .data$B19001_016E + .data$B19001_017E,
      IncomeDisparity = log(100 * (.data$B19001_002E / .data$incomegreater50)),
      less150poverty  = .data$C17002_002E + .data$C17002_003E +
        .data$C17002_004E + .data$C17002_005E,
      less150FPL      = .data$less150poverty / .data$C17002_001E,
      singlePchildren = .data$B11005_005E / .data$B11005_002E,
      novehiclesum      = .data$B25044_003E + .data$B25044_010E,
      pnovehicle      = .data$novehiclesum / .data$B25044_001E,
      sumprofs        = .data$C24010_003E + .data$C24010_039E,
      whitecollar     = .data$sumprofs / .data$C24010_001E,
      unemployedPct   = .data$B23025_005E / .data$B23025_003E,
      Phighschoolup   = .data$Nhighschoolup / .data$B15003_001E,
      Pless9grade     = .data$Nless9thgrade / .data$B15003_001E,
      SUMcrowded      = .data$B25014_005E + .data$B25014_006E +
        .data$B25014_007E + .data$B25014_011E + .data$B25014_012E +
        .data$B25014_013E,
      Pcrowded        = .data$SUMcrowded / .data$B25014_001E
    ) %>%
    
    dplyr::select(
      "medianFamilyIncome"                            = "B19113_001E",
      "medianMortgage"                                = "B25088_002E",
      "medianRent"                                    = "B25064_001E",
      "medianHouseValue"                              = "B25077_001E" ,
      "pctFamiliesInPoverty"                          = "Fpoverty",
      "pctOwnerOccupiedHousing"                       = "OwnerOcc",
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = "IncomeDisparity",
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = "less150FPL",
      "pctChildrenInSingleParentHouseholds"           = "singlePchildren",
      "pctHouseholdsWithNoVehicle"                    = "pnovehicle",
      "pctPeopleWithWhiteCollarJobs"                  = "whitecollar",
      "pctPeopleUnemployed"                           = "unemployedPct",
      "pctPeopleWithAtLeastHSEducation"               = "Phighschoolup",
      "pctPeopleWithLessThan9thGradeEducation"        = "Pless9grade",
      "pctHouseholdsWithOverOnePersonPerRoom"         = "Pcrowded"
    )
}
 

#' @importFrom rlang .data
factors_from_2000_decennial <- function(data) {
  # Selects the relevant variables from the tidycensus::get_acs() output, then
  # wrangles them into a data frame that contains the specific measures that are
  # used to calculate ADI
  data %>%
    
    as.data.frame() %>%
    # In case data is an sf tibble, this causes the geometry column to become
    # "unsticky", allowing it to be removed by the subsequent dplyr::select()
    # command so that it doesn't interfere with the imputation that may follow.
    
    dplyr::mutate(
      Fpoverty        = .data$P090002 / .data$P090001,
      OwnerOcc        = .data$H007002 / .data$H007001,
      incomegreater50 = .data$P052011 + .data$P052012 + .data$P052013 +
        .data$P052014 + .data$P052015 + .data$P052016 + .data$P052017,
      IncomeDisparity = log(100 * (.data$P052002 / .data$incomegreater50)),
      less150poverty  = .data$P088002 + .data$P088003 + .data$P088004 +
        .data$P088005 + .data$P088006,
      less150FPL      = .data$less150poverty / .data$P088001,
      oneparent       = .data$P046008 + .data$P046021,
      singlePchildren = .data$oneparent / .data$P046001,
      novehiclesum      = .data$H044003 + .data$H044010,
      pnovehicle      = .data$novehiclesum / .data$H044001,
      sumprofs        = .data$P050003 + .data$P050050,
      whitecollar     = .data$sumprofs / .data$P050001,
      unemployedLabor = .data$P043007 + .data$P043014,
      allLabor        = .data$P043005 + .data$P043012,
      unemployedPct   = .data$unemployedLabor / .data$allLabor,
      Nhighschoolup   = .data$P037011 + .data$P037028 + .data$P037012 +
        .data$P037029 + .data$P037013 + .data$P037030 + .data$P037014 +
        .data$P037031 + .data$P037015 + .data$P037032 + .data$P037016 +
        .data$P037033 + .data$P037017 + .data$P037034 + .data$P037018 +
        .data$P037035,
      Phighschoolup   = .data$Nhighschoolup / .data$P037001,
      Nless9thgrade   = .data$P037003 + .data$P037020 + .data$P037004 +
        .data$P037021 + .data$P037005 + .data$P037022 +
        .data$P037006 + .data$P037023,
      Pless9grade     = .data$Nless9thgrade / .data$P037001,
      SUMcrowded      = .data$H020005 + .data$H020006 + .data$H020007 +
        .data$H020011 + .data$H020012 + .data$H020013,
      Pcrowded        = .data$SUMcrowded / .data$H020001
    ) %>%
    
    dplyr::select(
      "medianFamilyIncome"                            = "P077001",
      "medianMortgage"                                = "H091001",
      "medianRent"                                    = "H063001",
      "medianHouseValue"                              = "H085001" ,
      "pctFamiliesInPoverty"                          = "Fpoverty",
      "pctOwnerOccupiedHousing"                       = "OwnerOcc",
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = "IncomeDisparity",
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = "less150FPL",
      "pctChildrenInSingleParentHouseholds"           = "singlePchildren",
      "pctHouseholdsWithNoVehicle"                    = "pnovehicle",
      "pctPeopleWithWhiteCollarJobs"                  = "whitecollar",
      "pctPeopleUnemployed"                           = "unemployedPct",
      "pctPeopleWithAtLeastHSEducation"               = "Phighschoolup",
      "pctPeopleWithLessThan9thGradeEducation"        = "Pless9grade",
      "pctHouseholdsWithOverOnePersonPerRoom"         = "Pcrowded"
    )
}


#' @importFrom rlang .data
factors_from_1990_decennial <- function(data) {
  data %>% 
    as.data.frame() %>% 
    
    dplyr::mutate(
      familybelowpoverty = .data$P1230013 + .data$P1230014 + .data$P1230015 +
        .data$P1230016 + .data$P1230017 + .data$P1230018 + .data$P1230019 +
        .data$P1230020 + .data$P1230021 + .data$P1230022 + .data$P1230023 +
        .data$P1230024,
      Fpoverty = .data$familybelowpoverty / .data$P0040001,
      OwnerOcc = .data$H0080001 / .data$H0040001,
      incomeunder10 = .data$P0800001 + .data$P0800002,
      incomeover50 = .data$P0800019 + .data$P0800020 + .data$P0800021 +
        .data$P0800022 + .data$P0800023 + .data$P0800024 + .data$P0800025,
      IncomeDisparity = log(100 * .data$incomeunder10 / .data$incomeover50),
      less150poverty = .data$P1210001 + .data$P1210002 + .data$P1210003 +
        .data$P1210004 + .data$P1210005,
      personspovertydetermined = .data$less150poverty + .data$P1210006 +
        .data$P1210007 + .data$P1210008 + .data$P1210009,
      less150FPL = .data$less150poverty / .data$personspovertydetermined,
      oneparent = .data$P0230008 + .data$P0230009 + .data$P0230010 +
        .data$P0230011 + .data$P0230012 + .data$P0230013 + .data$P0230014 +
        .data$P0230015 + .data$P0230016 + .data$P0230017 + .data$P0230018 +
        .data$P0230019 + .data$P0230020 + .data$P0230021,
      allchildren = .data$oneparent + .data$P0230001 + .data$P0230002 +
        .data$P0230003 + .data$P0230004 + .data$P0230005 + .data$P0230006 +
        .data$P0230007,
      singlePchildren = .data$oneparent / .data$allchildren,
      novehiclesum = .data$H0410001 + .data$H0410003,
      vehiclesdetermined = .data$novehiclesum + .data$H0410002 + .data$H0410004,
      pnovehicle = .data$novehiclesum / .data$vehiclesdetermined,
      sumprofs = .data$P0780001 + .data$P0780002,
      allcivilianemployed = .data$P0700002 + .data$P0700006,
      whitecollar = .data$sumprofs / .data$allcivilianemployed,
      unemployedlabor = .data$P0700003 + .data$P0700007,
      allLabor = .data$unemployedlabor + .data$P0700002 + .data$P0700006,
      unemployedPct = .data$unemployedlabor / .data$allLabor,
      Nhighschoolup = .data$P0570003 + .data$P0570004 + .data$P0570005 +
        .data$P0570006 + .data$P0570007,
      people25andover = .data$Nhighschoolup + .data$P0570001 + .data$P0570002,
      Phighschoolup = .data$Nhighschoolup / .data$people25andover,
      Pless9grade = .data$P0570001 / .data$people25andover,
      SUMcrowded = .data$H0710003 + .data$H0710004 + .data$H0710007 +
        .data$H0710008,
      Pcrowded = .data$SUMcrowsed / .data$H0040001
    ) %>% 
    
    dplyr::select(
      "medianHouseholdIncome"                         = "P080A001",
      "medianMortgage"                                = "H052A001",
      "medianRent"                                    = "H043A001",
      "medianHouseValue"                              = "H061A001",
      "pctFamiliesInPoverty"                          = "Fpoverty",
      "pctOwnerOccupiedHousing"                       = "OwnerOcc",
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = "IncomeDisparity",
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = "less150FPL",
      "pctChildrenInSingleParentHouseholds"           = "singlePchildren",
      "pctHouseholdsWithNoVehicle"                    = "pnovehicle",
      "pctPeopleWithWhiteCollarJobs"                  = "whitecollar",
      "pctPeopleUnemployed"                           = "unemployedPct",
      "pctPeopleWithAtLeastHSEducation"               = "Phighschoolup",
      "pctPeopleWithLessThan9thGradeEducation"        = "Pless9grade",
      "pctHouseholdsWithOverOnePersonPerRoom"         = "Pcrowded"
    )
}
