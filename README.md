# tidySDOH

The goal of tidySDOH is to help the user to operationalize social determinants of health data in their research.

Currently, we have implemented a variation of Singh's area deprivation index (ADI), which allows for estimation at different levels of spatial resolution and which allows for using different iterations of data from the American Community Survey (ACS).

The result is a more flexible framework for representing neighborhood deprivation. The get_adi() function is the primary tool for generating these indices. It allows the user to customize the desired reference area down to the block group level when calculating ADI. This enables the user to compare only the specific locations of interest without having to include other areas in the calculation of ADI. The output of get_adi() can be piped directly into ggplot2::geom_sf() for mapping.

## Installation

You can install the released version of tidySDOH from Github at https://github.com/NikKrieger/tidySDOH.

## Background on ADI

The main feature of tidySDOH is the get_adi() function, which returns a table of area deprivation indices (ADIs) for specific census designations that the user provides. These tables can be piped directly into ggplot2::geom_sf() for mapping.

In short,

"The Area Deprivation Index (ADI) is based on a measure created by the Health Resources & Services Administration (HRSA) over two decades ago for primarily county-level use, but refined, adapted, and validated to the Census block group/neighborhood level by Amy Kind, MD, PhD and her research team at the University of Wisconsin-Madison. It allows for rankings of neighborhoods by socioeconomic status disadvantage in a region of interest (e.g. at the state or national level)."

(This quotation and more information on the ADI can be found at https://www.neighborhoodatlas.medicine.wisc.edu/ )

The algorithm that determines the ADI of a specific location employs factor analysis. As a result, the ADI of a specific location is a relative measure, depending on which other locations are supplied to the algorithm. In other words, ADI will vary depending on the "reference area." For example, if the user wants the ADI of Orange County in California, he or she must decide whether the reference area shall be all counties in California, only Southern Califoria counties, all counties in the contiguous US, and so on. 

## Main Features

The function `get_adi()` can currently calculate ADIs for states, counties, census tracts, and census block groups. It stands on the shoulders of the `get_acs()` function in Kyle Walker's `tidycensus` package (see https://walkerke.github.io/tidycensus ). As such, the user can select specific years and specific editions of the Survey (i.e., the ACS's one-, three-, or five-year estimates) in calculating ADI, as long as they exist and are available through the American Community Survey.

## Examples

This code would return the ADI of each of the 50 states plus the District of Columbia and Puerto Rico, using the 2012 edition of the 5-year ACS estimates:

`get_adi(geography = "state", year = 2012)`

This code would return the ADI of all counties in Connecticut, using the 2010 edition of the 1-year ACS estimates:

`get_adi(geography = "county", state = "CT", year = 2015, survey = "acs1")`

The user can mix of different levels of geography in the `geoids` parameter. This code would return the ADI of every county entirely or partially on the Delmarva peninsula, using the 5-year ACS estimates from 2016 (the defaults).

`(delmarva <- get_adi(geoids = c("10", "51001", "51131", "24015", "24029", "24035", "24011", "24041", "24019", "24045", "24039", "24047")))`

The table produced by `get_adi()` can then be piped directly into `geom_sf()` from the `ggplot2` package, producing the plot below.

`delmarva %>% ggplot() + geom_sf(aes(fill = ADI))`

![Delmarva Peninsula](https://github.com/NikKrieger/tidySDOH/raw/master/tools/readme/Delmarva.png)

## Demonstration of the relative nature of ADI

The code below calculates and maps ADIs for Ohio counties. 

`ohio <- get_adi(geography = "county", state = "OH")`
`ohio %>% ggplot(aes(fill = ADI)) + geom_sf()`

The code below also calculates and maps ADIs for Ohio counties, but it uses all counties in Ohio, Michigan, and Indiana as the reference area:

`ohmiin <- get_adi(geography = "county", state = c("OH", "MI", "IN")) %>%`
`  filter(as.integer(GEOID) >= 39000)`
`ohmiin %>% ggplot(aes(fill = ADI)) + geom_sf()`

## Warning about missing data

While allowing flexibility in specifying reference areas, data from the ACS are masked for small levels of geography and may have too many missing values to return ADIs in some cases. 

Also, know when to use the ACS1, ACS3, or ACS5. See https://www.census.gov/programs-surveys/acs/guidance/estimates.html.
