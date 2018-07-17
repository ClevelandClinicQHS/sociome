# tidySDOH

The goal of tidySDOH is to help the user to operationalize social determinants of health data in their research.

Currently, we have implemented a variation of Singh's area deprivation index (ADI), which allows for estimation at different levels of spatial resolution and which allows for using different iterations of data from the American Community Survey (ACS).

The result is a more flexible framework for representing neighborhood deprivation. The `get_adi()` function is the primary tool for generating these indices. It allows the user to customize the desired reference area down to the block group level when calculating ADI. This enables the user to compare only the specific locations of interest without having to include other areas in the calculation of ADI.

The output of `get_adi()` can be piped directly into `ggplot2::geom_sf()` for mapping.

## Installation

You can install the released version of tidySDOH from Github using:

```
devtools::install_github("NikKrieger/tidySDOH")
```

## Background on ADI

The main feature of tidySDOH is the `get_adi()` function, which returns a table of "customized" Singh's area deprivation indices (ADIs) for specific census designations that the user provides.

Note that the "original" or "true" ADIs are static measures that were calculated using the 2013 edition of the ACS five-year estimates. These ADIs are available at https://www.neighborhoodatlas.medicine.wisc.edu/download. This package calculates and returns indices using the same algorithms used to calculate the "true" ADIs, but it accomplishes this using user-specified ACS data.

In short,

> "The Area Deprivation Index (ADI) is based on a measure created by the Health Resources & Services Administration (HRSA) over two decades ago for primarily county-level use, but refined, adapted, and validated to the Census block group/neighborhood level by Amy Kind, MD, PhD and her research team at the University of Wisconsin-Madison. It allows for rankings of neighborhoods by socioeconomic status disadvantage in a region of interest (e.g. at the state or national level)."

(This quotation and more information on the ADI can be found at https://www.neighborhoodatlas.medicine.wisc.edu)

The algorithm that determines the ADI of a specific location employs factor analysis. As a result, the ADI of a specific location is a relative measure, depending on which other locations are supplied to the algorithm. In other words, ADI will vary depending on the "reference area." For example, if the user wants the ADI of Orange County in California, he or she must decide whether the reference area shall be all counties in California, only Southern Califoria counties, all counties in the contiguous US, and so on. 

## Main Features

The function `get_adi()` can currently calculate ADIs for states, counties, census tracts, and census block groups. It stands on the shoulders of the `get_acs()` function in Kyle Walker's `tidycensus` package (see https://walkerke.github.io/tidycensus). As such, the user can select specific years and specific editions of the Survey (i.e., the ACS's one-, three-, or five-year estimates) in calculating ADI, as long as they exist and are available through the American Community Survey.

## Examples

The code below would return the ADI of each of the 50 states plus the District of Columbia and Puerto Rico, using the 2012 edition of the 5-year ACS estimates:

```
get_adi(geography = "state", year = 2012)
```

The code below returns the ADI of all counties in Connecticut, using the 2010 edition of the 1-year ACS estimates and excluding the `geometry` column that allows for mapping:

```
get_adi(geography = "county", state = "CT",
        year = 2015, survey = "acs1", geometry = FALSE)
```

Here is that table in full:

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> GEOID </th>
   <th style="text-align:left;"> NAME </th>
   <th style="text-align:right;"> ADI </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 09001 </td>
   <td style="text-align:left;"> Fairfield County, Connecticut </td>
   <td style="text-align:right;"> 98.81834 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09003 </td>
   <td style="text-align:left;"> Hartford County, Connecticut </td>
   <td style="text-align:right;"> 117.45380 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09005 </td>
   <td style="text-align:left;"> Litchfield County, Connecticut </td>
   <td style="text-align:right;"> 82.26225 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09007 </td>
   <td style="text-align:left;"> Middlesex County, Connecticut </td>
   <td style="text-align:right;"> 78.28255 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09009 </td>
   <td style="text-align:left;"> New Haven County, Connecticut </td>
   <td style="text-align:right;"> 128.75087 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09011 </td>
   <td style="text-align:left;"> New London County, Connecticut </td>
   <td style="text-align:right;"> 113.90137 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09013 </td>
   <td style="text-align:left;"> Tolland County, Connecticut </td>
   <td style="text-align:right;"> 75.21250 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 09015 </td>
   <td style="text-align:left;"> Windham County, Connecticut </td>
   <td style="text-align:right;"> 105.31825 </td>
  </tr>
</tbody>
</table>

The user can mix of different levels of geography in the `geoids` parameter. The code below stores the ADI of every county entirely or partially on the Delmarva peninsula, using by default the 5-year ACS estimates from 2016. 

```
delmarva_geoids <- c("10", "51001", "51131", "24015", "24029", "24035", "24011",
                     "24041", "24019", "24045", "24039", "24047")
  # The two-digit FIPS code stands for the state of Delaware.
  # The five-digit FIPS codes stand for counties in Virginia and Maryland

delmarva <- get_adi(geoids = delmarva_geoids)
  # The Delmarva peninsula lies on the east coast of the US and is split
  #   between DELaware, MARyland, and VirginiA.
```

By employing `tidycensus::get_acs()`, tables produced by `get_adi()` contain a column named `geometry`, which contains `sf` data gathered from the US census API. As a result, `get_adi()` tables can be piped directly into `geom_sf()` from the `ggplot2` package. Below is a demonstration of this capability, using the table of Delmarva peninsula county ADIs created above:

```
delmarva %>% ggplot() + geom_sf(aes(fill = ADI))
```

![](https://raw.githubusercontent.com/NikKrieger/tidySDOH/master/man/figures/Delmarva.png)


### Demonstration of the relative nature of ADI

The code below calculates and maps ADIs for Ohio counties. 

```
ohio <- get_adi(geography = "county", state = "OH")

ohio %>% ggplot() + geom_sf(aes(fill = ADI))
```

![](https://raw.githubusercontent.com/NikKrieger/tidySDOH/master/man/figures/Ohio_counties_ADI_ref_area_OH_counties.png)


The code below also calculates and maps ADIs for Ohio counties, but it uses a reference area of all counties in the fifty states plus DC and Puerto Rico:

```
ohio_ref_US <- get_adi(geography = "county") %>%
  filter(substr(GEOID, 1, 2) == "39")
  # Ohio's GEOID is 39, so the GEOIDs of all Ohio counties begin with 39.

ohio_ref_US %>% ggplot() + geom_sf(aes(fill = ADI))
```

![](https://raw.githubusercontent.com/NikKrieger/tidySDOH/master/man/figures/Ohio_counties_ADI_ref_area_US_counties.png)

Notice how the ADI of each county varies depending on the reference area provided.

## Warning about missing data

While allowing flexibility in specifying reference areas, data from the ACS are masked for sparsely populated places and may have too many missing values to return ADIs in some cases. 

Also, know when to use the ACS1, ACS3, or ACS5. See https://www.census.gov/programs-surveys/acs/guidance/estimates.html.
