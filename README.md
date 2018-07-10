# tidySDOH

The goal of tidySDOH is to help the user to view and analyze social determinants of health data in the American Community Survey.

## Installation

You can install the released version of tidySDOH from Github at \url{https://github.com/NikKrieger/tidySDOH}.

## Background on ADI

The main feature of tidySDOH is the get_adi() function, which returns a table of area deprivation indices (ADIs) for specific census designations that the user provides. 

In short,

"The Area Deprivation Index (ADI) is based on a measure created by the Health Resources & Services Administration (HRSA) over two decades ago for primarily county-level use, but refined, adapted, and validated to the Census block group/neighborhood level by Amy Kind, MD, PhD and her research team at the University of Wisconsin-Madison. It allows for rankings of neighborhoods by socioeconomic status disadvantage in a region of interest (e.g. at the state or national level)."

(This quotation and more information on the ADI can be found at \url{https://www.neighborhoodatlas.medicine.wisc.edu/}.)

The algorithm that determines the ADI of a specific location employs factor analysis. As a result, the ADI of a specific location is a relative measure, depending on which other locations are supplied to the algorithm. In other words, ADI will vary depending on the "reference area." For example, if the user wants the ADI of Orange County in California, he or she must decide whether the reference area shall be all counties in California, only Southern Califoria counties, all counties in the contiguous US, and so on. 

## Main Features

The get_adi() function allows the user to customize the desired reference area down to the block group level when calculating ADI. This enables the user to compare only the specific locations of interest without having to include other areas in the calculation of ADI.

Since the package employs American Community Survey data, the user can also select specific years and specific editions of the Survey (i.e., the ACS's 1-, 3-, or 5-year estimates) in calculating ADI. 

ADI can currently be calculated for states, counties, census tracts, and census block groups.

## Example

This code would return the ADI of each of the 50 states plus the District of Columbia and Puerto Rico, using the 2012 edition of the 5-year ACS estimates:

`get_adi(geography = "state", year = 2012)`

This code would return the ADI of all counties in Connecticut, using the 2010 edition of the 1-year ACS estimates:

`get_adi(geography = "county", state = "CT", year = 2010, survey = "acs1")`

This code would return the ADI of roughly every census tract (the default) in the Delmarva peninsula (some tracts not actually on the Delmarva peninsula in the extreme north of Delaware and in Cecil County, Maryland may be inadvertently included), using the 5-year ACS estimates from 2016 (the defaults). Notice the mixing of different levels of geography in the `geoids` parameter:

`get_adi(geoids = c("10", "24015", "24029", "24035", "24011", "24041", "24019", "24045", "24039", "24047", "51001", "51131"))`
