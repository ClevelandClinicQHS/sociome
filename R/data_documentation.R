
#' ACS variable names for ADI calculation
#'
#' A dataset of the ACS variable names used to calculate the Area Deprivation
#' Index (ADI).
#'
#' @format A \code{\link[tibble]{tibble}} with 137 rows and 6 variables:
#'   \describe{ \item{variable}{ACS variable name} \item{description}{Brief
#'   description of the data the variable contains}
#'   \item{B23025_and_B15003}{Logical, indicating the variables to be used when
#'   calculating ADI using the 1- or 3-year estimates from 2011 and later or
#'   when using the 5-year estimates from 2012 or later}
#'   \item{B23025_and_B15002}{Logical, indicating the variables to be used when
#'   calculating ADI using the 2011 5-year estimates}
#'   \item{B23001_and_B15003}{Logical, indicating the variables to be used when
#'   calculating ADI using the 2008-2010 1-year estimates or the 2010 3-year
#'   estimates} \item{B23001_and_B15002}{Logical, indicating the variables to be
#'   used when calculating ADI using the pre-2008 1-year estimates, the pre-2010
#'   3-year estimates, or the pre-2011 5-year estimates} }
"acs_vars"