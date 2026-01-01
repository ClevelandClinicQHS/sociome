
#' ACS variable names for ADI and ADI-3 calculation
#'
#' A dataset of the ACS variable names used to calculate the Area Deprivation
#' Index (ADI) and Berg Indices (ADI-3).
#'
#' @format A [`tibble`][tibble::tibble] with 139 rows and 10 variables:
#'
#' - **variable**: ACS variable name.
#'
#' - **description**: Brief description of the data the variable contains.
#'
#' - **set1**: Logical,
#'   indicating the variables to be used when calculating ADI and ADI-3 using
#'   the 1- or 3-year estimates from 2011 and later or when using the 5-year
#'   estimates from 2012 or later, with the exception of requesting
#'   block-group-level data from 2015 or 2016.
#'
#' - **set2**: Logical, indicating the variables
#'   to be used when calculating ADI and ADI-3 at the block group level using
#'   the 2015 or 2016 estimates.
#'
#' - **set3**: Logical, indicating the variables
#'   to be used when calculating ADI using the 2011 5-year estimates.
#'
#' - **set4**: Logical, indicating the variables to be used when calculating
#'   ADI and ADI-3 using the 2010 1- or 3-year estimates.
#'
#' - **set5**: Logical,
#'   indicating the variables to be used when calculating ADI and ADI-3 using
#'   the 2010 5-year estimates.
#'
#' - **set6**: Logical, indicating the variables to
#'   be used when calculating ADI and ADI-3 using the 2008 or 2009 1-year
#'   estimates.
#'
#' - **set7**: Logical, indicating the variables to be used when
#'   calculating ADI and ADI-3 estimates not previously mentioned, including the
#'   2009 5-year estimates.
#'
#' - **dec2010**: Logical, indicating the ACS variables to be used
#'   when calculating ADI and ADI-3 estimates using 2010 or 2020 decennial
#'   census data.
#'
#'   Note that not all year/estimate combinations are currently supported by the
#'   census API and/or \code{tidycensus}, and some may never be supported.
#'
#' @seealso [`decennial_vars`] and [`dataset_year_geography_availability`]
"acs_vars"



#' Decennial census variable names for ADI calculation
#'
#' A dataset of the decennial census variable names used to calculate the Area
#' Deprivation Index (ADI) and the Berg Indices (ADI-3).
#'
#' @format A [`tibble`][tibble::tibble] with 144 rows and 4 variables:
#'
#' - **variable**: Decennial census variable name.
#'
#' - **sumfile**: The summary tape file of the decennial census variable.
#'
#' - **year**: The year of the decennial census variable.
#'
#' - **description**: Brief description of the data the variable contains.
#'
#' @seealso [tidycensus::get_decennial()], [`acs_vars`], and
#'   [`dataset_year_geography_availability`]
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
#' @format A [`tibble`][tibble::tibble] with 65 rows and 2 variables:
#'
#' - **variable**: ACS variable name.
#'
#' - **description**: A description of who is present in the count.
#'
#' @seealso [synthetic_population()] and
#'   [`decennial_age_sex_race_ethnicity_vars`]
"acs_age_sex_race_ethnicity_vars"




#' Decennial Census variables for age, sex, race, and ethnicity
#'
#' A three-column data set of the Decennial Census variable names, their
#' descriptions, and their decennial census year. Contains counts of various
#' subdivisions of the population based on age, sex, race, and ethnicity.
#'
#' Currently, the 2000, 2010, and 2020 Decennial Census variables are available.
#'
#' This data set is used to support \code{\link{synthetic_population}()}.
#'
#' @format A [`tibble`][tibble::tibble] with 195 rows and 3 variables:
#'
#' - **year**: The year of the decennial census with which the
#'   variable is associated.
#'
#' - **variable**: ACS variable name.
#'
#' - **description**: A description of who is present in the count.
#'
#' @seealso [synthetic_population()] and
#'   [`acs_age_sex_race_ethnicity_vars`]
"decennial_age_sex_race_ethnicity_vars"


#' Working Combinations of `dataset`, `year`, and `geography` in [get_adi()]
#'
#' A three-column data set of combinations of the `dataset`, `year`, and
#' `geography` arguments to [get_adi()] that should successfully yield ADI and
#' ADI-3 estimates.
#'
#' @format A [`tibble`][tibble::tibble] with 130 rows and 4 variables:
#'
#' - **dataset**: Value that can be supplied to the `dataset` argument of [get_adi()].
#'
#' - **year**: Value that can be supplied to the `year` argument of [get_adi()].
#'
#' - **geography**: Value that can be supplied to the `geography` argument of [get_adi()]. Note that `"zip code tabulation area"` is not represented here: just use `"zcta"`, please.
#'
#' - **acs**: Logical, indicating that the combination of variables can yield ADIs and ADI-3s.
#'
#' @seealso [get_adi()]
"dataset_year_geography_availability"



#' Vector of the two-digit GEOIDs for the 50 states, DC and Puerto Rico
#'
#' 52-element named character vector of the two-digit GEOIDs of the 50 states,
#' DC and Puerto Rico. The names are the spelled-out names of each place.
#'
#' These are derived from [tidycensus::fips_codes]. Other territories not
#' mentioned here are simply filtered out.
#' @examples
#' state_geoids
"state_geoids"
