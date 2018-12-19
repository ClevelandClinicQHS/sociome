#' @importFrom rlang .data
calculate_adi <- function(data, type, keep_indicators) {
  
  if(!is.data.frame(data)) {
    stop("data must be a tibble, sf tibble, or data-frame-like object")
  }
  
  if(nrow(data) < 30) {
    warning("\nCalculating ADI values from fewer than 30 locations.\nIt is ",
            "recommended to add more in order to obtain trustworthy results.")
  }
  
  if(type == "decennial") {
    data_f <- factors_from_decennial(data)
  }
  else {
    data_f <- factors_from_acs(data)
  }
  
  # Performs single imputation if there is any missingness in the data.
  if(anyNA(data_f)) {
    
    # Error message if user has not attached the sociome and/or mice package.
    # Attaching one of these packages is necessary because of how mice::mice
    # operates. See ?sociome::mice.impute.pmm for details.
    if(!any(c("package:sociome", "package:mice") %in% search())) {
      stop(paste0('Imputation required. Run library(sociome) or ',
                  'library(mice) and try again.'))
    }
    
    is.na(data_f) <- do.call(cbind, lapply(data_f, is.infinite))
    
    impute_attempt <- 
      try(data_f %>%
            mice::mice(m = 1, maxit = 50, method = "pmm", seed = 500,
                       printFlag = FALSE) %>%
            mice::complete())
    
    if(class(impute_attempt) == "try-error") {
      message("\nImputation unsuccessful.\nReturning factors that could not ",
              "be imputed followed by raw census data.")
      return(
        data %>% 
          as.data.frame() %>% 
          dplyr::bind_cols(data_f, .) %>% 
          dplyr::select("GEOID", "NAME", tidyselect::everything()) %>% 
          tibble::as_tibble()
      )
    }
    
    data_f <- impute_attempt
    
    message("\nSingle imputation performed")
  }
  
  if(keep_indicators) {
    data         <- dplyr::bind_cols(data, data_f)
    keep_columns <- append(names(data), "ADI", after = 2)
  }
  else {
    keep_columns <- c("GEOID", "NAME", "ADI")
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
  signage_flipper <- sign(sum(sign(fit$loadings) * c(-1, -1, -1, -1, 1, -1, 1,
                                                     1, 1, 1, -1, 1, -1, 1, 1)))
  #   4. The variable signage_flipper is multiplied by the PCA scores before
  #      standardization. In effect, this flips the ADIs in the right direction
  #      (multiplies their scores by -1) if they were reversed, and it keeps
  #      them the same (multiplies their scores by 1) if they were not reversed.
  # The raw ADI scores are standardized to have a mean of 100 and sd of 20
  adi <- data %>%
    dplyr::mutate(ADI = as.numeric(fit$scores * signage_flipper * 20 + 100)) %>%
    dplyr::select(keep_columns)
  
  if(!("sf" %in% class(adi))) {
    adi <- tibble::as_tibble(adi)
  }
  
  attr(adi, "loadings") <- stats::setNames(object = as.vector(fit$loadings),
                                           nm     = row.names(fit$loadings))
  
  class(adi) <- c(class(adi), "adi")
  
  return(adi)
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
  
  if(is.null(data$B23025_005E)) {
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
        B23025_001E = .data$B23001_006E + .data$B23001_013E +
          .data$B23001_020E + .data$B23001_027E + .data$B23001_034E +
          .data$B23001_041E + .data$B23001_048E + .data$B23001_055E +
          .data$B23001_062E + .data$B23001_069E + .data$B23001_074E +
          .data$B23001_079E + .data$B23001_084E + .data$B23001_092E +
          .data$B23001_099E + .data$B23001_106E + .data$B23001_113E +
          .data$B23001_120E + .data$B23001_127E + .data$B23001_134E +
          .data$B23001_141E + .data$B23001_148E + .data$B23001_155E +
          .data$B23001_160E + .data$B23001_165E + .data$B23001_170E)
  }
  
  if(is.null(data$B15002_001E)) {
    data <- data %>% 
      dplyr::mutate(
        Nless9thgrade = .data$B15003_002E +  .data$B15003_003E + 
          .data$B15003_004E + .data$B15003_005E + .data$B15003_006E + 
          .data$B15003_007E + .data$B15003_008E + .data$B15003_009E + 
          .data$B15003_010E + .data$B15003_011E + .data$B15003_012E,
        Nhighschoolup = .data$B15003_017E + .data$B15003_018E +
          .data$B15003_019E + .data$B15003_020E + .data$B15003_021E +
          .data$B15003_022E + .data$B15003_023E + .data$B15003_024E +
          .data$B15003_025E)
  }
  else {
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
        B15003_001E   = .data$B15002_001E)
  }
  
  data <- data %>% 
    dplyr::mutate(
      Fpoverty        = .data$B17010_002E / .data$B17010_001E,
      OwnerOcc        = .data$B25003_002E / .data$B25003_001E,
      incomegreater50 = .data$B19001_011E + .data$B19001_012E +
        .data$B19001_013E + .data$B19001_014E +
        .data$B19001_015E + .data$B19001_016E +
        .data$B19001_017E,
      IncomeDisparity = log(100 * (.data$B19001_002E / .data$incomegreater50)),
      less150poverty  = .data$C17002_002E + .data$C17002_003E +
        .data$C17002_004E + .data$C17002_005E,
      less150FPL      = .data$less150poverty / .data$C17002_001E,
      oneparent       = .data$B23008_008E + .data$B23008_021E,
      singlePHH       = .data$oneparent / .data$B23008_001E,
      vehiclesum      = .data$B25044_003E + .data$B25044_010E,
      pnovehicle      = .data$vehiclesum / .data$B25044_001E,
      sumprofs        = .data$C24010_003E + .data$C24010_039E,
      whitecollar     = .data$sumprofs / .data$C24010_001E,
      unemployedPct   = .data$B23025_005E / .data$B23025_001E,
      Phighschoolup   = .data$Nhighschoolup / .data$B15003_001E,
      Pless9grade     = .data$Nless9thgrade / .data$B15003_001E,
      SUMcrowded      = .data$B25014_005E + .data$B25014_006E +
        .data$B25014_007E + .data$B25014_011E +
        .data$B25014_012E + .data$B25014_013E,
      Ocrowded        = .data$SUMcrowded / .data$B25014_001E) %>%
    dplyr::select(
      "medianHouseholdIncome"                         = .data$B19013_001E,
      "medianMortgage"                                = .data$B25088_002E,
      "medianRent"                                    = .data$B25064_001E,
      "medianHouseValue"                              = .data$B25077_001E ,
      "pctFamiliesInPoverty"                          = .data$Fpoverty,
      "pctOwnerOccupiedHousing"                       = .data$OwnerOcc,
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = .data$IncomeDisparity,
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = .data$less150FPL,
      "pctChildrenInSingleParentHouseholds"           = .data$singlePHH ,
      "pctHouseholdsWithNoVehicle"                    = .data$pnovehicle,
      "pctPeopleWithWhiteCollarJobs"                  = .data$whitecollar,
      "pctPeopleUnemployed"                           = .data$unemployedPct,
      "pctPeopleWithAtLeastHSEducation"               = .data$Phighschoolup,
      "pctPeopleWithLessThan9thGradeEducation"        = .data$Pless9grade,
      "pctHouseholdsWithOverOnePersonPerRoom"         = .data$Ocrowded)
}
 

#' @importFrom rlang .data
factors_from_decennial <- function(data) {
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
      incomegreater50 = .data$P052011 + .data$P052012 +
        .data$P052013 + .data$P052014 +
        .data$P052015 + .data$P052016 +
        .data$P052017,
      IncomeDisparity = log(100 * (.data$P052002 / .data$incomegreater50)),
      less150poverty  = .data$P088002 + .data$P088003 + .data$P088004 +
        .data$P088005 + .data$P088006,
      less150FPL      = .data$less150poverty / .data$P088001,
      oneparent       = .data$P046008 + .data$P046021,
      singlePHH       = .data$oneparent / .data$P046001,
      vehiclesum      = .data$H044003 + .data$H044010,
      pnovehicle      = .data$vehiclesum / .data$H044001,
      sumprofs        = .data$P050003 + .data$P050050,
      whitecollar     = .data$sumprofs / .data$P050001,
      unemployedLabor = .data$P043007 + .data$P043014,
      allLabor        = .data$P043005 + .data$P043012,
      unemployedPct   = unemployedLabor / allLabor,
      Nhighschoolup   = .data$P037011 + .data$P037028 + .data$P037012 +
        .data$P037029 + .data$P037013 + .data$P037030 +
        .data$P037014 + .data$P037031 + .data$P037015 +
        .data$P037032 + .data$P037016 + .data$P037033 +
        .data$P037017 + .data$P037034 + .data$P037018 +
        .data$P037035,
      Phighschoolup   = .data$Nhighschoolup / .data$P037001,
      Nless9thgrade   = .data$P037003 + .data$P037020 + .data$P037004 +
        .data$P037021 + .data$P037005 + .data$P037022 +
        .data$P037006 + .data$P037023,
      Pless9grade     = .data$Nless9thgrade / .data$P037001,
      SUMcrowded      = .data$H020005 + .data$H020006 + .data$H020007 +
        .data$H020011 + .data$H020012 + .data$H020013,
      Ocrowded        = .data$SUMcrowded / .data$H020001) %>%
    dplyr::select(
      "medianHouseholdIncome"                         = .data$P053001,
      "medianMortgage"                                = .data$H091001,
      "medianRent"                                    = .data$H063001,
      "medianHouseValue"                              = .data$H085001 ,
      "pctFamiliesInPoverty"                          = .data$Fpoverty,
      "pctOwnerOccupiedHousing"                       = .data$OwnerOcc,
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = .data$IncomeDisparity,
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = .data$less150FPL,
      "pctChildrenInSingleParentHouseholds"           = .data$singlePHH ,
      "pctHouseholdsWithNoVehicle"                    = .data$pnovehicle,
      "pctPeopleWithWhiteCollarJobs"                  = .data$whitecollar,
      "pctPeopleUnemployed"                           = .data$unemployedPct,
      "pctPeopleWithAtLeastHSEducation"               = .data$Phighschoolup,
      "pctPeopleWithLessThan9thGradeEducation"        = .data$Pless9grade,
      "pctHouseholdsWithOverOnePersonPerRoom"         = .data$Ocrowded)
}



#' \code{mice.impute.pmm} from the \code{mice} package
#'
#' This is only visible in the sociome package out of necessity so that
#' imputation works.
#'
#' See \code{\link[mice]{mice.impute.pmm}} for its actual documentation.
#'
#' The reason that this function is in the visible exported namespace of sociome
#' is as follows:
#' \code{mice::mice()} internally calls one or more of the imputation method
#' functions in the \code{mice} package (in this case,
#' \code{mice.impute.pmm()}), but \code{mice::mice} looks for these functions in
#' an unconventional way: it looks only in the global environment and all its
#' parent environments. Therefore, the mice package must be attached in order
#' for \code{mice::mice()} to work. Instead of forcing the user to attach
#' \code{mice} whenever sociome needs to utilize \code{mice::mice()}, we elected
#' to import the imputation method function that we used
#' (\code{mice.impute.pmm()}) into the exported namespace of sociome. In effect,
#' this lets the user choose whether to attach the sociome package OR the
#' \code{mice} package (e.g., running \code{library(sociome)} or
#' \code{library(mice)}).
#'
#' @name mice.impute.pmm
#' @export
#' @importFrom mice mice.impute.pmm
NULL
