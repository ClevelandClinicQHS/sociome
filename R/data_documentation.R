
#' ACS variable names for ADI calculation
#'
#' A dataset of the ACS variable names used to calculate the Area Deprivation
#' Index (ADI).
#'
#' @format A \code{\link[tibble]{tibble}} with 138 rows and 8 variables:
#'   \describe{ \item{variable}{ACS variable name} \item{description}{Brief
#'   description of the data the variable contains} \item{set1}{Logical,
#'   indicating the variables to be used when calculating ADI using the 1- or
#'   3-year estimates from 2011 and later or when using the 5-year estimates
#'   from 2012 or later} \item{set2}{Logical, indicating the variables to be
#'   used when calculating ADI at the block group level using the 2015 or 2016
#'   estimates} \item{set3}{Logical, indicating the variables to be used when
#'   calculating ADI using the 2011 5-year estimates} \item{set4}{Logical,
#'   indicating the variables to be used when calculating ADI using the
#'   2008-2010 1-year estimates or the 2010 3-year estimates}
#'   \item{set5}{Logical, indicating the variables to be used when calculating
#'   ADI using the pre-2008 1-year estimates, the pre-2010 3-year estimates, or
#'   the pre-2011 5-year estimates} \item{dec2010}{Logical, indicating the
#'   variables to use in conjunction with the few actual 2010 decennial census
#'   variables when running \code{\link{get_adi}(year = 2010, dataset =
#'   "decennial")}}}
#'
#' @seealso \code{\link{decennial_vars}}
"acs_vars"



#' Decennial census variable names for ADI calculation
#'
#' A dataset of the decennial census variable names used to calculate the Area
#' Deprivation Index (ADI).
#'
#' @format A \code{\link[tibble]{tibble}} with 137 rows and 4 variables:
#'   \describe{ \item{variable}{Decennial census variable name}
#'   \item{sumfile}{The summary tape file of the decennial census variable}
#'   \item{year}{The year of the decennial census variable}
#'   \item{description}{Brief description of the data the variable contains}}
#'   
#' @seealso \code{\link{acs_vars}}
"decennial_vars"
