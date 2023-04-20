
#' ACS variable names for ADI and ADI-3 calculation
#'
#' A dataset of the ACS variable names used to calculate the Area Deprivation
#' Index (ADI) and Berg Indices (ADI-3).
#'
#' @format A [`tibble`][tibble::tibble] with 139 rows and 10 variables:
#'   \describe{ \item{variable}{ACS variable name} \item{description}{Brief
#'   description of the data the variable contains} \item{set1}{Logical,
#'   indicating the variables to be used when calculating ADI and ADI-3 using
#'   the 1- or 3-year estimates from 2011 and later or when using the 5-year
#'   estimates from 2012 or later} \item{set2}{Logical, indicating the variables
#'   to be used when calculating ADI and ADI-3 at the block group level using
#'   the 2015 or 2016 estimates} \item{set3}{Logical, indicating the variables
#'   to be used when calculating ADI using the 2011 5-year estimates}
#'   \item{set4}{Logical, indicating the variables to be used when calculating
#'   ADI and ADI-3 using the 2010 1- or 3-year estimates} \item{set5}{Logical,
#'   indicating the variables to be used when calculating ADI and ADI-3 using
#'   the 2010 5-year estimates} \item{set6}{Logical, indicating the variables to
#'   be used when calculating ADI and ADI-3 using the 2008 or 2009 1-year
#'   estimates} \item{set7}{Logical, indicating the variables to be used when
#'   calculating ACS estimates not previously mentioned, including the 2009
#'   5-year estimates} \item{dec2010}{Logical, indicating the variables to use
#'   in conjunction with the few actual 2010 decennial census variables when
#'   running \code{\link{get_adi}(year = 2010, dataset = "decennial")}}}
#'
#'   Note that not all year/estimate combinations are currently supported by the
#'   census API and/or \code{tidycensus}, and some may never be supported.
#'
#' @seealso \code{\link{decennial_vars}}
#' @noMd
"acs_vars"



#' Decennial census variable names for ADI calculation
#'
#' A dataset of the decennial census variable names used to calculate the Area
#' Deprivation Index (ADI) and the Berg Indices (ADI-3).
#'
#' @format A \code{\link[tibble]{tibble}} with 137 rows and 4 variables:
#'   \describe{ \item{variable}{Decennial census variable name}
#'   \item{sumfile}{The summary tape file of the decennial census variable}
#'   \item{year}{The year of the decennial census variable}
#'   \item{description}{Brief description of the data the variable contains}}
#'   
#' @seealso \code{\link{acs_vars}}
#' @noMd
"decennial_vars"



#' ACS variables for age, sex, race, and ethnicity
#'
#' A two-column data set of the American Community Survey variable names and
#' their descriptions. Contains counts of various subdivisions of the population
#' based on age, sex, race, and ethnicity.
#'
#' These variable names have been consistent throughout the existence of the ACS
#' from its beginning through 2020.
#' 
#' This data set is used to support \code{\link{synthetic_population}()}.
#'
#' @format A \code{\link[tibble]{tibble}} with 65 rows and 2 variables:
#'   \describe{ \item{variable}{ACS variable name}
#'   \item{description}{A description of who is present in the count}}
#'
#' @seealso \code{\link{decennial_age_sex_race_ethnicity_vars}}
#' @noMd
"acs_age_sex_race_ethnicity_vars"




#' Decennial Census variables for age, sex, race, and ethnicity
#'
#' A three-column data set of the Decennial Census variable names, their
#' descriptions, and their decennial census year. Contains counts of various
#' subdivisions of the population based on age, sex, race, and ethnicity.
#'
#' Currently, the 2000 and 2010 Decennial Census variables are available.
#'
#' This data set is used to support \code{\link{synthetic_population}()}.
#'
#' @format A \code{\link[tibble]{tibble}} with 130 rows and 3 variables:
#'   \describe{ \item{year}{The year of the decennial census with which the
#'   variable is associated.} \item{variable}{ACS variable name}
#'   \item{description}{A description of who is present in the count}}
#'
#' @seealso \code{\link{acs_age_sex_race_ethnicity_vars}}
#' @noMd
"decennial_age_sex_race_ethnicity_vars"
