#' GEOIDs of all US states, counties, tracts, and block groups.
#' 
#' Data set containing the GEOIDs (FIPS codes) of every state, county, tract,
#' and block group in the US.
#'
#' Data frame in which each row represents a block group. Columns contain the
#' block groups' corresponding state-, county-, tract-, and block-group-level
#' GEOIDs (FIPS codes), along with an extra column containing the county-level
#' FIPS code with the first two digits (i.e., the state GEOID) removed.
#'
#' Adapted from the data file at the URL below.
#'
#' @format A data frame with 220334 rows and 5 columns:
#' \describe{
#'   \item{block group}{12-digit FIPS code of all census block groups}
#'   \item{tract}{11-digit FIPS code of block group's corresponding census
#'   tract}
#'   \item{county}{5-digit FIPS code of block group's corresponding county}
#'   \item{state}{2-digit FIPS code of block group's corresponding state}
#'   \item{short_county}{3-digit FIPS code of block group's corresponding
#'   county}
#' }
#'
#' @source
#'   \url{https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG.txt}
#'   and
#'   \url{https://raw.githubusercontent.com/NikKrieger/sociome/master/data-raw/us_block_groups_integer_vector_creation.Rmd}
#'   
"fips_table"

#' @export
fips_table <- NULL