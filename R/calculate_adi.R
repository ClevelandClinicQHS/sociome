#' Calculate ADI from census data.
#'
#' Calculate the area deprivation index using decennial US census or American
#' Community Survey (ACS) variables.
#'
#' The function \code{\link{get_adi}()} calls this function by default as its
#' final step, but some users may want to calculate ADI values for different
#' combinations of areas in a given data set. \code{\link{get_adi}(raw_data_only
#' = TRUE)} returns the raw census data used to calculate ADI. Users may select
#' subsets of such a data set and pipe them into \code{calculate_adi()}.
#'
#' This function discerns what kind of census data that \code{data} contains
#' (ACS, or one of the decennial censuses) by checking for the existence of key
#' variables unique to each kind of data set.
#'
#' Areas listed as having zero households are excluded from ADI calculation.
#' Their resulting ADIs will be \code{NA}.
#'
#' If calling this function directly (i.e., not via \code{get_adi()}) on a data
#' set that contains median household income (B19013_001) and does not contain
#' median family income (B19113_001), median household income will be used in
#' place of median family income, with a \code{warning()}. See the "Missingness
#' and imputation" section of \code{\link{get_adi}()}.
#'
#' @seealso For more information, see \code{\link{get_adi}()}, especially
#'   \strong{ADI factor loadings} and \strong{Missingness and imputation}.
#'
#' @param data_raw A data frame, \code{\link[tibble]{tibble}}, or
#'   \code{\link[sf]{sf}} ultimately obtained via
#'   \code{tidycensus::\link[tidycensus]{get_acs}()} or
#'   \code{tidycensus::\link[tidycensus]{get_decennial}()}, having the data
#'   necessary to compute the indicators of the ADI.
#'
#'   The columns of his data frame must be named according to the elements of
#'   the \code{variable} column in \code{sociome::\link{acs_vars}} and/or
#'   \code{sociome::decennial_vars}.
#'
#'   The easiest way to obtain data like this is to run
#'   \code{sociome::\link{get_adi}(raw_data_only = TRUE)}.
#' @param keep_indicators Logical indicating whether or not to keep the
#'   component indicators of the ADI as well as the original census variables
#'   used to calculate them. Defaults to \code{FALSE}.
#'
#'   See \code{\link{acs_vars}} and \code{\link{decennial_vars}} for basic
#'   descriptions of the raw census variables.
#'
#' @param seed Passed to the \code{seed} argument of
#'   \code{mice::\link[mice]{mice}()} when imputation is needed.
#'
#' @return A \code{\link[tibble]{tibble}} with the same number of rows as
#'   \code{data}. Columns include \code{GEOID}, \code{NAME}, and \code{ADI}.
#'   Further columns containing the indicators and raw values will also be
#'   present if \code{keep_indicators = TRUE}.
#'
#' @examples
#' \dontrun{
#' # Wrapped in \dontrun{} because these examples require a Census API key.
#'
#' raw_census <- get_adi("state", raw_data_only = TRUE)
#'
#' calculate_adi(raw_census)
#'
#' calculate_adi(raw_census, keep_indicators = TRUE)
#' }
#' @importFrom rlang .data
#' @export
calculate_adi <- function(data_raw, keep_indicators = FALSE, seed = NA) {
  
  if (!is.data.frame(data_raw)) {
    stop("data must be a tibble, sf tibble, or data-frame-like object")
  }
  
  total_hh_colname <- get_total_hh_colname(data_raw)
  
  nonzero_hh_lgl <- data_raw[[total_hh_colname]] != 0
  
  indicators <- calculate_indicators(data_raw)
  
  indicators_hh_only <- indicators %>% dplyr::filter(nonzero_hh_lgl)
  
  if (nrow(indicators_hh_only) < 30L) {
    warning(
      "\nCalculating ADI values from fewer than 30 locations.\nIt is ",
      "recommended to add more in order to obtain trustworthy results."
    )
  }
  
  # Performs single imputation if there is any missingness in the data.
  if (anyNA(indicators_hh_only)) {
    indicators_hh_only <-
      tryCatch(
        indicators_hh_only %>%
          mice::mice(
            m = 1L, 
            maxit = 50L, 
            method = "pmm", 
            seed = seed,
            printFlag = FALSE
          ) %>%
          mice::complete(),
        error = function(e) {
          rlang::abort(
            paste0(
              "Imputation unsuccessful. ADIs not calculated.",
              "\n\nRun rlang::last_error()$adi_indicators to access the ",
              "indicator data",
              "\nwhose missingness could not be imputed. These data exclude ",
              "areas with zero households.",
              "\n\nRun rlang::last_error()$adi_raw_data to access the raw ",
              "census data,\n",
              "which includes areas with zero households (identified by the ",
              'column named "', total_hh_colname, '").'
            ),
            .subclass = "imputation_unsuccessful",
            adi_indicators = 
              data_raw %>% 
              dplyr::as_tibble() %>% 
              dplyr::select("GEOID", dplyr::starts_with("NAME")) %>% 
              dplyr::filter(nonzero_hh_lgl) %>% 
              dplyr::bind_cols(indicators_hh_only),
            adi_raw_data =
              data_raw %>%
              dplyr::select(
                "GEOID",
                dplyr::starts_with("NAME"),
                !!total_hh_colname,
                dplyr::everything()
              )
          )
        }
      )
    
    indicators[nonzero_hh_lgl, ] <- indicators_hh_only
    
    message("\nSingle imputation performed")
  }
  
  adi <-
    purrr::map2_dfc(
      list(
        ADI = TRUE,
        Financial_Strength =
          c("medianFamilyIncome", "medianMortgage", "medianRent",
            "medianHouseValue", "pctPeopleWithWhiteCollarJobs"),
        Economic_Hardship_and_Inequality =
          c("pctFamiliesInPoverty", "pctOwnerOccupiedHousing",
            "ratioThoseMakingUnder10kToThoseMakingOver50k",
            "pctPeopleLivingBelow150PctFederalPovertyLevel",
            "pctHouseholdsWithChildrenThatAreSingleParent",
            "pctHouseholdsWithNoVehicle", "pctPeopleUnemployed"),
        Educational_Attainment = 
          c("pctPeopleWithAtLeastHSEducation",
            "pctPeopleWithLessThan9thGradeEducation",
            "pctHouseholdsWithOverOnePersonPerRoom")
      ),
      list(
        c(-1, -1, -1, -1, 1, -1, 1, 1, 1, 1, -1, 1, -1, 1, 1),
        c(-1, -1, -1, -1, -1),
        c(1, -1, 1, 1, 1, 1, 1),
        c(-1, 1, 1)
      ),
      function(set, expected_signs, result_vec) {
        
        # Where the magic happens: a principal-components analysis (PCA) of the
        # statistics that produces the raw ADI scores
        fit <- psych::principal(indicators_hh_only[set])
        
        # Sometimes the PCA produces results that are completely reversed (i.e.,
        # it gives deprived areas low ADIs and less deprived areas high ADIs).
        # Therefore, this function performs a check to see if this has occurred.
        
        # 1. The signage of the factor loadings are multiplied by their expected
        # signage according to Singh's original research (present in the unnamed
        # vector of 1s and -1s below). This produces a vector of 1s and -1s,
        # with a 1 indicating a factor loading in the expected direction and a
        # -1 indicating a factor loading in the wrong direction.
        
        # 2. The sum() of this vector is computed.
        
        # 3. The sign() of this sum is computed and saved into a variable called
        # "signage_flipper". It will equal 1 or -1. It will equal 1 if most of
        # the factor loadings have the same sign as the original Singh factor
        # loadings. It will be -1 if not. It will never equal 0 because there is
        # an odd number of factor loadings.
        signage_flipper <- sign(sum(sign(fit$loadings) * expected_signs))
        #   4. The variable signage_flipper is multiplied by the PCA scores
        #   before standardization. In effect, this flips the ADIs in the right
        #   direction (multiplies their scores by -1) if they were reversed, and
        #   it keeps them the same (multiplies their scores by 1) if they were
        #   not reversed.
        
        # The raw ADI scores are standardized to have a mean of 100 and sd of 20
        result_vec[nonzero_hh_lgl] <-
          as.numeric(fit$scores * signage_flipper * 20 + 100)
        
        # We also want the loadings tables for each of the three factors
        attr(result_vec, "loadings") <-
          dplyr::tibble(
            factor = row.names(fit$loadings),
            loading = as.double(fit$loadings)
          )
        
        result_vec
      },
      result_vec = rep_len(NA_real_, length.out = length(nonzero_hh_lgl))
    )
  
  out <-
    if (keep_indicators) {
      dplyr::select(
        dplyr::bind_cols(data_raw, adi, indicators),
        1L,
        2L,
        !!colnames(adi),
        !!colnames(indicators),
        !!total_hh_colname,
        dplyr::everything()
      )
    } else dplyr::bind_cols(data_raw[1L:2L], adi)
  
  attr(out, "loadings") <- attr(out$ADI, "loadings")
  
  class(out) <- c("adi", class(out))
  
  out
}



get_total_hh_colname <- function(data_raw) {
  
  total_hh_colname <-
    colnames(data_raw) %>%
    `[`(. %in% c("P018001", "P015001", "P0030001", "B11005_001"))
  
  if (length(total_hh_colname) != 1L) {
    stop(
      "As of sociome 1.1.0, the data set must have exactly one of the",
      "\nfollowing columns, so that zero-houshold areas may be removed:",
      "\n\nP018001, P015001, P0030001, B11005_001"
    )
  }
  
  total_hh_colname
}



calculate_indicators <- function(data) {
  
  colnames <- colnames(data)
  
  indicators <-
    if (any(colnames == "B17010_001")) {
      factors_from_acs(data, colnames)
    } else if (any(colnames == "P077001")) {
      factors_from_2000_decennial(data)
    } else if (any(colnames == "P107A001")) {
      factors_from_1990_decennial(data)
    } else {
      stop("Data missing at least one variable necessary to calculate ADI.")
    }
  
  indicators[!is.finite(as.matrix.data.frame(indicators))] <- NA
  
  indicators
}



# Selects the relevant variables from the tidycensus::get_acs() output, then
# wrangles them into a data frame that contains the specific measures that are
# used to calculate ADI
#' @importFrom rlang .data
factors_from_acs <- function(data_raw, colnames) {
  
  data_indicators <- as.data.frame(data_raw)
  # In case data is an sf tibble, this causes the geometry column to become
  # "unsticky", allowing it to be removed by the subsequent dplyr::select()
  # command so that it doesn't interfere with the imputation that may follow.
  
  if (any(colnames == "C24010_040")) {
    if (any(colnames == "C24010_039")) {
      warning(
        "\nThe variables C24010_039 and C24010_040 are both present.",
        '\nC24010_039 will be used for "civilian females age 16+ in',
        '\nwhite-collar occupations", which is incorrect for pre-2010 data.',
        "\nIf seeking pre-2010 estimates, remove C24010_039 from dataset.",
        call. = FALSE,
        immediate. = TRUE
      )
    } else {
      data_indicators <- data_indicators %>% 
        dplyr::rename("C24010_039" = "C24010_040")
    }
  }
  
  if (any(colnames == "B19013_001") && !any(colnames == "B19113_001")) {
    
    if (!any(grepl("B19113_001", names(warnings())))) {
      warning(
        "\nMedian household income (B19013_001) is being used in place of ",
        "\nmedian family income (B19113_001).",
        "\n\nThis is desirable only for 2015 or 2016 block group-level data.",
        '\n\nSee the "Missingness and imputation" section of ?get_adi, ',
        "as well as:",
        "\nhttps://www.census.gov/programs-surveys/acs/",
        "technical-documentation/user-notes/2016-01.html",
        call. = FALSE,
        immediate. = TRUE
      )
    }
    
    data_indicators <- data_indicators %>% 
      dplyr::rename("B19113_001" = "B19013_001")
    
    median_income_name <- "medianHouseholdIncome"
  } else {
    median_income_name <- "medianFamilyIncome"
  }
  
  if (!any(colnames == "B25003_001")) {
    data_indicators <- data_indicators %>% 
      dplyr::rename(
        "B25003_001"      = "H003002",
        "B25003_002"      = "H014002",
        "B11005_002"      = "P020002",
        "B11005_005"      = "P020008"
      )
  }
  
  if (!any(colnames == "B23025_005")) {
    data_indicators <- data_indicators %>% 
      dplyr::mutate(
        B23025_005 = .data$B23001_008 + .data$B23001_015 +
          .data$B23001_022 + .data$B23001_029 + .data$B23001_036 +
          .data$B23001_043 + .data$B23001_050 + .data$B23001_057 +
          .data$B23001_064 + .data$B23001_071 + .data$B23001_076 +
          .data$B23001_081 + .data$B23001_086 + .data$B23001_094 +
          .data$B23001_101 + .data$B23001_108 + .data$B23001_115 +
          .data$B23001_122 + .data$B23001_129 + .data$B23001_136 +
          .data$B23001_143 + .data$B23001_150 + .data$B23001_157 +
          .data$B23001_162 + .data$B23001_167 + .data$B23001_172,
        B23025_003 = .data$B23001_006 + .data$B23001_013 +
          .data$B23001_020 + .data$B23001_027 + .data$B23001_034 +
          .data$B23001_041 + .data$B23001_048 + .data$B23001_055 +
          .data$B23001_062 + .data$B23001_069 + .data$B23001_074 +
          .data$B23001_079 + .data$B23001_084 + .data$B23001_092 +
          .data$B23001_099 + .data$B23001_106 + .data$B23001_113 +
          .data$B23001_120 + .data$B23001_127 + .data$B23001_134 +
          .data$B23001_141 + .data$B23001_148 + .data$B23001_155 +
          .data$B23001_160 + .data$B23001_165 + .data$B23001_170
      )
  }
  
  if (any(colnames == "B15002_001")) {
    data_indicators <- data_indicators %>% 
      dplyr::mutate(
        Nless9thgrade = .data$B15002_003 +  .data$B15002_020 +
          .data$B15002_004 +  .data$B15002_021 +  .data$B15002_005 + 
          .data$B15002_022 +  .data$B15002_006 +  .data$B15002_023,
        Nhighschoolup = .data$B15002_011 + .data$B15002_028 +
          .data$B15002_012 +  .data$B15002_029 + .data$B15002_013 +
          .data$B15002_030 + .data$B15002_014 + .data$B15002_031 +
          .data$B15002_015 + .data$B15002_032 + .data$B15002_016 +
          .data$B15002_033 + .data$B15002_017 + .data$B15002_034 +
          .data$B15002_018 + .data$B15002_035,
        B15003_001   = .data$B15002_001
      )
  } else {
    data_indicators <- data_indicators %>% 
      dplyr::mutate(
        Nless9thgrade = .data$B15003_002 +  .data$B15003_003 + 
          .data$B15003_004 + .data$B15003_005 + .data$B15003_006 + 
          .data$B15003_007 + .data$B15003_008 + .data$B15003_009 + 
          .data$B15003_010 + .data$B15003_011 + .data$B15003_012,
        Nhighschoolup = .data$B15003_017 + .data$B15003_018 +
          .data$B15003_019 + .data$B15003_020 + .data$B15003_021 +
          .data$B15003_022 + .data$B15003_023 + .data$B15003_024 +
          .data$B15003_025
      )
  }
  
  data_indicators %>% 
    
    dplyr::mutate(
      Fpoverty        = .data$B17010_002 / .data$B17010_001,
      OwnerOcc        = .data$B25003_002 / .data$B25003_001,
      incomegreater50 = .data$B19001_011 + .data$B19001_012 +
        .data$B19001_013 + .data$B19001_014 + .data$B19001_015 +
        .data$B19001_016 + .data$B19001_017,
      IncomeDisparity = log(100 * (.data$B19001_002 / .data$incomegreater50)),
      less150poverty  = .data$C17002_002 + .data$C17002_003 +
        .data$C17002_004 + .data$C17002_005,
      less150FPL      = .data$less150poverty / .data$C17002_001,
      singlePchildren = .data$B11005_005 / .data$B11005_002,
      novehiclesum    = .data$B25044_003 + .data$B25044_010,
      pnovehicle      = .data$novehiclesum / .data$B25044_001,
      sumprofs        = .data$C24010_003 + .data$C24010_039,
      whitecollar     = .data$sumprofs / .data$C24010_001,
      unemployedPct   = .data$B23025_005 / .data$B23025_003,
      Phighschoolup   = .data$Nhighschoolup / .data$B15003_001,
      Pless9grade     = .data$Nless9thgrade / .data$B15003_001,
      SUMcrowded      = .data$B25014_005 + .data$B25014_006 +
        .data$B25014_007 + .data$B25014_011 + .data$B25014_012 +
        .data$B25014_013,
      Pcrowded        = .data$SUMcrowded / .data$B25014_001
    ) %>%
    
    dplyr::select(
      !!median_income_name                           := "B19113_001",
      "medianMortgage"                                = "B25088_002",
      "medianRent"                                    = "B25064_001",
      "medianHouseValue"                              = "B25077_001" ,
      "pctFamiliesInPoverty"                          = "Fpoverty",
      "pctOwnerOccupiedHousing"                       = "OwnerOcc",
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = "IncomeDisparity",
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = "less150FPL",
      "pctHouseholdsWithChildrenThatAreSingleParent"  = "singlePchildren",
      "pctHouseholdsWithNoVehicle"                    = "pnovehicle",
      "pctPeopleWithWhiteCollarJobs"                  = "whitecollar",
      "pctPeopleUnemployed"                           = "unemployedPct",
      "pctPeopleWithAtLeastHSEducation"               = "Phighschoolup",
      "pctPeopleWithLessThan9thGradeEducation"        = "Pless9grade",
      "pctHouseholdsWithOverOnePersonPerRoom"         = "Pcrowded"
    )
}


#' @importFrom rlang .data
factors_from_2000_decennial <- function(data_raw) {
  # Selects the relevant variables from the tidycensus::get_acs() output, then
  # wrangles them into a data frame that contains the specific measures that are
  # used to calculate ADI
  
  data_raw %>% 
    
    as.data.frame() %>%
    # In case data is an sf tibble, this causes the geometry column to become
    # "unsticky", allowing it to be removed by the subsequent dplyr::select()
    # command so that it doesn't interfere with the imputation that may follow.
    
    dplyr::mutate(
      Fpoverty        = .data$P090002 / .data$P090001,
      OwnerOcc        = .data$H004002 / .data$H004001,
      incomegreater50 = .data$P052011 + .data$P052012 + .data$P052013 +
        .data$P052014 + .data$P052015 + .data$P052016 + .data$P052017,
      IncomeDisparity = log(100 * (.data$P052002 / .data$incomegreater50)),
      less150poverty  = .data$P088002 + .data$P088003 + .data$P088004 +
        .data$P088005 + .data$P088006,
      less150FPL      = .data$less150poverty / .data$P088001,
      singlePchildren = .data$P019005 / .data$P019002,
      novehiclesum    = .data$H044003 + .data$H044010,
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
      "pctHouseholdsWithChildrenThatAreSingleParent"  = "singlePchildren",
      "pctHouseholdsWithNoVehicle"                    = "pnovehicle",
      "pctPeopleWithWhiteCollarJobs"                  = "whitecollar",
      "pctPeopleUnemployed"                           = "unemployedPct",
      "pctPeopleWithAtLeastHSEducation"               = "Phighschoolup",
      "pctPeopleWithLessThan9thGradeEducation"        = "Pless9grade",
      "pctHouseholdsWithOverOnePersonPerRoom"         = "Pcrowded"
    )
}


#' @importFrom rlang .data
factors_from_1990_decennial <- function(data_raw) {
  
  data_raw %>% 
    
    as.data.frame() %>% 
    
    dplyr::mutate(
      familybelowpoverty = .data$P1230013 + .data$P1230014 + .data$P1230015 +
        .data$P1230016 + .data$P1230017 + .data$P1230018 + .data$P1230019 +
        .data$P1230020 + .data$P1230021 + .data$P1230022 + .data$P1230023 +
        .data$P1230024,
      Fpoverty = .data$familybelowpoverty / .data$P0040001,
      OwnerOcc = .data$H0030001 / .data$H0020001,
      incomeunder10 = .data$P0800001 + .data$P0800002,
      incomeover50 = .data$P0800019 + .data$P0800020 + .data$P0800021 +
        .data$P0800022 + .data$P0800023 + .data$P0800024 + .data$P0800025,
      IncomeDisparity = log(100 * .data$incomeunder10 / .data$incomeover50),
      less150poverty = .data$P1210001 + .data$P1210002 + .data$P1210003 +
        .data$P1210004 + .data$P1210005,
      personspovertydetermined = .data$less150poverty + .data$P1210006 +
        .data$P1210007 + .data$P1210008 + .data$P1210009,
      less150FPL = .data$less150poverty / .data$personspovertydetermined,
      oneparent = .data$P0180002 + .data$P0180003,
      allchildren = .data$oneparent + .data$P0180001 + .data$P0180004 +
        .data$P0180005,
      singlePchildren = .data$oneparent / .data$allchildren,
      novehiclesum = .data$H0410001 + .data$H0410003,
      vehiclesdetermined = .data$novehiclesum + .data$H0410002 + .data$H0410004,
      pnovehicle = .data$novehiclesum / .data$vehiclesdetermined,
      sumprofs = .data$P0780001 + .data$P0780002,
      allcivilianemployed = .data$P0700002 + .data$P0700006,
      whitecollar = .data$sumprofs / .data$allcivilianemployed,
      unemployedlabor = .data$P0700003 + .data$P0700007,
      allLabor = .data$unemployedlabor + .data$allcivilianemployed,
      unemployedPct = .data$unemployedlabor / .data$allLabor,
      Nhighschoolup = .data$P0570003 + .data$P0570004 + .data$P0570005 +
        .data$P0570006 + .data$P0570007,
      people25andover = .data$Nhighschoolup + .data$P0570001 + .data$P0570002,
      Phighschoolup = .data$Nhighschoolup / .data$people25andover,
      Pless9grade = .data$P0570001 / .data$people25andover,
      SUMcrowded = .data$H0210003 + .data$H0210004 + .data$H0210005,
      crowdingdetermined = .data$SUMcrowded + .data$H0210001 + .data$H0210002,
      Pcrowded = .data$SUMcrowded / .data$crowdingdetermined
    ) %>% 
    
    dplyr::select(
      "medianFamilyIncome"                            = "P107A001",
      "medianMortgage"                                = "H052A001",
      "medianRent"                                    = "H043A001",
      "medianHouseValue"                              = "H023B001",
      "pctFamiliesInPoverty"                          = "Fpoverty",
      "pctOwnerOccupiedHousing"                       = "OwnerOcc",
      "ratioThoseMakingUnder10kToThoseMakingOver50k"  = "IncomeDisparity",
      "pctPeopleLivingBelow150PctFederalPovertyLevel" = "less150FPL",
      "pctHouseholdsWithChildrenThatAreSingleParent"  = "singlePchildren",
      "pctHouseholdsWithNoVehicle"                    = "pnovehicle",
      "pctPeopleWithWhiteCollarJobs"                  = "whitecollar",
      "pctPeopleUnemployed"                           = "unemployedPct",
      "pctPeopleWithAtLeastHSEducation"               = "Phighschoolup",
      "pctPeopleWithLessThan9thGradeEducation"        = "Pless9grade",
      "pctHouseholdsWithOverOnePersonPerRoom"         = "Pcrowded"
    )
}
