#' Calculate area deprivation index (ADI)
#'
#' Calculates ADIs from raw ACS data.
#'
#' Returns a tibble or sf tibble of area deprivation indices (ADIs), calculating
#' them from a data frame of raw ACS data gathered from the US Census API
#' (preferrably via \code{tidycensus::get_acs}).
#'
#' The returned tibble or sf tibble is also of class \code{adi}, and it contains
#' an attribute called \code{loadings}, which contains a named numeric vector of
#' the PCA loadings of each factor. This is accessible through
#' \code{attr(name_of_tibble, "loadings")}.
#'
#' @param acs_data A data-frame-like object of ACS data from the US Census API.
#'   Must have the variables described in the "Details" section below.
#' @param keep_indicators Logical value indicating whether or not the resulting
#'   tibble (or sf tibble) will contain the socioeconomic measures used to
#'   calculate the ADI values.
#' @param keep_columns Character string vector of the names of columns in
#'   acs_data to keep in the final result. Defaults to \code{c("GEOID",
#'   "NAME")}. If acs_data is of class \code{sf}, the geometry column will be
#'   kept regardless.
#'
#' @details The argument acs_data must contain a tibble, sf tibble, data frame,
#'   or equivalent containing the following columns:
#'
#'   \code{c("B01003_001E", "B19013_001E", "B19001_002E", "B19001_011E",
#'   "B19001_012E", "B19001_013E", "B19001_014E", "B19001_015E", "B19001_016E",
#'   "B19001_017E", "B17010_001E", "B17010_002E", "B25003_001E", "B25003_002E",
#'   "C17002_001E", "C17002_002E", "C17002_003E", "C17002_004E", "C17002_005E",
#'   "B25044_001E", "B25044_003E", "B25044_010E", "B25014_001E", "B25014_005E",
#'   "B25014_006E", "B25014_007E", "B25014_011E", "B25014_012E", "B25014_013E",
#'   "B25088_001E", "B25064_001E", "B25077_001E", "C24010_001E", "C24010_003E",
#'   "C24010_039E", "B23025_001E", "B23025_005E", "B15003_001E", "B15003_002E",
#'   "B15003_003E", "B15003_004E", "B15003_005E", "B15003_006E", "B15003_007E",
#'   "B15003_008E", "B15003_009E", "B15003_010E", "B15003_011E", "B15003_012E",
#'   "B15003_017E", "B15003_018E", "B15003_019E", "B15003_020E", "B15003_021E",
#'   "B15003_022E", "B15003_023E", "B15003_024E", "B15003_025E", "B23008_001E",
#'   "B23008_008E", "B23008_021E")}
#'
#'   To obtain these columns using \code{tidycensus::get_acs}, make sure to set
#'   the argument \code{output = "wide"}, and make sure that the argument
#'   \code{variables} includes at least the elements of the character string
#'   vector in the example \strong{below} (notice the lack of "E" at the end of
#'   each column name).
#'
#'   All other columns in \code{acs_data} that are not specified in
#'   \code{keep_columns} won't be present in the output. Exception: if
#'   \code{acs_data} is an \code{sf-class} object, the \code{geometry} column
#'   will remain in the output regardless of the contents of
#'   \code{keep_columns}.
#'
#'   If there are any missing values, single imputation will be attempted using
#'   the \code{mice} package. Because of how \code{mice} is coded, the user must
#'   attach either the \code{sociome} package or the \code{mice} package for
#'   imputation to work (e.g., run \code{library("sociome")} or
#'   \code{library("mice")} before running \code{get_adi}). See
#'   \code{\link{mice.impute.pmm}} for details.
#'
#'   Be advised that if too many missing values are present in \code{acs_data}
#'   (due to running \code{tidycensus::get_acs} on sparsely populated localities
#'   or for some other reason), ADIs might not be able to be obtained.
#'
#' @return A tibble (or an sf tibble if \code{acs_data} is itself an sf tibble)
#'   containing firstly the columns of \code{acs_data} specified by
#'   \code{keep_columns}, followed by a column called \code{ADI}, calculated
#'   from the data present in \code{acs_data}, followed by the indicators used
#'   to calculate the ADIs if \code{keep_indicators = TRUE} (followed by the
#'   \code{geometry} column if \code{acs_data} iss an sf tibble).
#'
#'   By default, this will be three columns: \code{GEOID}, \code{NAME}, and
#'   \code{ADI}. (Four columns, adding \code{geometry}, if \code{acs_data} is an
#'   sf tibble).
#'
#' @examples
#' \dontrun{
#' acs_vars <- c("B01003_001", "B19013_001", "B19001_002", "B19001_011",
#'               "B19001_012", "B19001_013", "B19001_014", "B19001_015",
#'               "B19001_016", "B19001_017", "B17010_001", "B17010_002",
#'               "B25003_001", "B25003_002", "C17002_001", "C17002_002",
#'               "C17002_003", "C17002_004", "C17002_005", "B25044_001",
#'               "B25044_003", "B25044_010", "B25014_001", "B25014_005",
#'               "B25014_006", "B25014_007", "B25014_011", "B25014_012",
#'               "B25014_013", "B25088_001", "B25064_001", "B25077_001",
#'               "C24010_001", "C24010_003", "C24010_039", "B23025_001",
#'               "B23025_005", "B15003_001", "B15003_002", "B15003_003",
#'               "B15003_004", "B15003_005", "B15003_006", "B15003_007",
#'               "B15003_008", "B15003_009", "B15003_010", "B15003_011",
#'               "B15003_012", "B15003_017", "B15003_018", "B15003_019",
#'               "B15003_020", "B15003_021", "B15003_022", "B15003_023",
#'               "B15003_024", "B15003_025", "B23008_001", "B23008_008",
#'               "B23008_021")
#'
#' connecticut_counties <- tidycensus::get_acs(geography = "county", variables = acs_vars,
#'                                             output = "wide", state = "CT")
#' 
#' calculate_adi(acs_data = connecticut_counties)
#' # This will produce one or more warnings because there are only 8 counties in
#' # Connecticut, and a warning is produced if ADI values are calculated using 
#' # less than 30 locations.
#' }
#'
#' @seealso get_adi
#'
#' @importFrom rlang .data
#'
#' @export
calculate_adi <- function(data,
                          type            = NULL,
                          keep_indicators = FALSE,
                          keep_columns    = c("GEOID", "NAME")) {
  
  if(!is.data.frame(data)) {
    stop("data must be a tibble, sf tibble, or data-frame-like object")
  }
  
  if(!all(keep_columns %in% colnames(data))) {
    stop("Not all columns specified by keep_columns are in data.")
  }
  
  type <- validate_type(type, data)
  
  if(nrow(data) < 30) {
    warning("\n\nCalculating ADI values from fewer than 30 locations.\nIt is ",
            "recommended to add more in order to obtain trustworthy results.\n")
  }
  
  if(type == "decennial") {
    data_f <- factors_from_decennial(data)
  }
  else {
    data_f <- factors_from_acs(data)
  }
  
  keep_columns <- c(keep_columns, "ADI")
  if(keep_indicators) {
    data         <- dplyr::bind_cols(data, data_f)
    keep_columns <- c(keep_columns, names(data_f))
  }
  
  # Performs single imputation if there is any missingness in the data.
  if(anyNA(data_f)) {
    
    # Error message if user has not attached the sociome and/or mice package.
    # Attaching one of these packages is necessary because of how mice::mice
    # operates. See ?sociome::mice.impute.pmm for details.
    if(!any(c("package:sociome", "package:mice") %in% search())) {
      stop(paste0('Imputation required. Run library("sociome") or ',
                  'library("mice") and try again.'))
    }
    
    is.na(data_f) <- do.call(cbind, lapply(data_f, is.infinite))
    
    data_f <- data_f %>%
      mice::mice(m = 1, maxit = 50, method = "pmm", seed = 500,
                 printFlag = FALSE) %>%
      mice::complete()
    
    message("\n\nSingle imputation performed\n\n")
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
