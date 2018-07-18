# sociome

> The dimensions of existence that are social.

The goal of the `sociome` package is to help the user to operationalize social determinants of health data in their research.

The current functionality is limited to measures of area deprivation, but we intend to expand into other elements of the "sociome."

We have implemented a variation of Singh's area deprivation index (ADI), which allows for estimation at the state, county, census tract, or census block group level and which allows for using different iterations of data from the American Community Survey (ACS).

The result is a more flexible framework for representing neighborhood deprivation. The `get_adi()` function is the primary tool for generating these indices. It allows the user to customize the desired **reference area** down to the block group level when calculating ADI. This enables the user to compare only the specific locations of interest without having to include other areas in the calculation of ADI. See the section called "Choosing a **Reference Area**" below for more detail.

The output of `get_adi()` can be piped directly into `ggplot2::geom_sf()` for mapping.

## Installation

You can install the released version of sociome from Github using:

```
devtools::install_github("NikKrieger/sociome")
``` 

## Background on ADI

In short,

> "The Area Deprivation Index (ADI) is based on a measure created by the Health Resources & Services Administration (HRSA) over two decades ago for primarily county-level use, but refined, adapted, and validated to the Census block group/neighborhood level by Amy Kind, MD, PhD and her research team at the University of Wisconsin-Madison. It allows for rankings of neighborhoods by socioeconomic status disadvantage in a region of interest (e.g. at the state or national level)."

<div style="text-align: right"> (see https://www.neighborhoodatlas.medicine.wisc.edu) </div>


The *original* ADIs are static measures that were calculated using the 2013 edition of the ACS five-year estimates. Rankings based on these ADIs are available via downloadable datasets at https://www.neighborhoodatlas.medicine.wisc.edu/download.

The original ADI of Kind et al. (2018) is defined as a national measure. In other words, it uses the United States as the **reference area**. A given area might have different ADI values depending on the choice of the reference area; for example, a census tract in an "upper-class" neighborhood in Milwaukee might have a lower ADI value if the index is computed using the Milwaukee area as the reference than it would if the index is computed using the United States as the reference. The `get_adi()` function flexibly allows for specifying the **reference area** for ADI estimation. See examples below.

## Choosing a **reference area**

The algorithm that produced the original ADIs employs factor analysis. As a result, the ADI is a relative measure; the ADI of a particular location is dynamic, varying depending on which other locations were supplied to the algorithm. In other words, ADI will vary depending on the **reference area**. 

For example, the ADI of Orange County, California is *x* when calculated alongside all other counties in California, but it is *y* when calculated alongside all counties in the US.

The `get_adi()` function enables the user to define a reference area by feeding a vector of GEOIDs to its `geoids` parameter (or alternatively for convenience, a vector of state abbreviations to its `state` parameter). The function then gathers data from those specified locations and performs calculations using their data alone.

## *Customizable* ADIs via `get_adi()`

The `get_adi()` function returns a table of *customized* Singh's area deprivation indices (ADIs). The user chooses:

- the level of geography whose ADIs are desired (viz., state, county, census tract, census block group)
- the year
- the ACS estimates (viz. the one-, three-, or five-year estimates)
- the **reference area** (see above).

The function then calls the specified ACS data sets and employs the same algorithms that were used to calculate the *original* ADIs, resulting in *customized* ADIs. It stands on the shoulders of the `get_acs()` function in Kyle Walker's `tidycensus` package (see https://walkerke.github.io/tidycensus), which is what enables the user to so easily select specific years and specific ACS estimates. `get_adi()` also benefits from the shapefile-gathering capabilities of `get_acs()`, which enables the user to create maps depicting ADI values with relative ease, thanks to `geom_sf()` in `ggplot2`.

## Examples

The code below would return the ADI of each of the 50 states plus the District of Columbia and Puerto Rico, using the 2012 edition of the 5-year ACS estimates:

```
get_adi(geography = "state", year = 2012)
```

The code below returns the ADIs of all counties in Connecticut, using the 2010 edition of the 1-year ACS estimates and excluding the `geometry` column that allows for mapping:

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

The user can mix different levels of geography in the `geoids` parameter. The code below stores the ADI of every county entirely or partially on the Delmarva peninsula, using by default the 5-year ACS estimates from 2016. 

```
delmarva_geoids <- c("10", "51001", "51131", "24015", "24029", "24035", "24011",
                     "24041", "24019", "24045", "24039", "24047")
  # The two-digit GEOID stands for the state of Delaware.
  # The five-digit GEOIDs stand for specific counties in Virginia and Maryland

delmarva <- get_adi(geoids = delmarva_geoids)
  # The Delmarva peninsula lies on the east coast of the US and is split
  #   between DELaware, MARyland, and VirginiA.
```

With the help of `tidycensus::get_acs()`, tables produced by `get_adi()` contain a column named `geometry`, which contains `sf` data gathered from the US census API. As a result, `get_adi()` tables can be piped directly into `geom_sf()` from the `ggplot2` package. Below is a demonstration of this capability, using the Delmarva peninsula county ADIs created above:

```
delmarva %>% ggplot() + geom_sf(aes(fill = ADI))
```

![](https://raw.githubusercontent.com/NikKrieger/sociome/master/man/figures/Delmarva.png)


### Demonstration of the relative nature of ADIs, using custom reference areas

The code below calculates and maps ADIs for Ohio counties. 

```
ohio <- get_adi(geography = "county", state = "OH")

ohio %>% ggplot() + geom_sf(aes(fill = ADI))
```

![](https://raw.githubusercontent.com/NikKrieger/sociome/master/man/figures/Ohio_counties_ADI_ref_area_OH_counties.png)


The code below also calculates and maps ADIs for Ohio counties, but it uses a reference area of all counties in the fifty states plus DC and Puerto Rico:

```
ohio_ref_US <- get_adi(geography = "county") %>%
  filter(substr(GEOID, 1, 2) == "39")
  # Ohio's GEOID is 39, so the GEOIDs of all Ohio counties begin with 39.

ohio_ref_US %>% ggplot() + geom_sf(aes(fill = ADI))
```

![](https://raw.githubusercontent.com/NikKrieger/sociome/master/man/figures/Ohio_counties_ADI_ref_area_US_counties.png)

Notice how the ADI of each county varies depending on the reference area provided.

## Warning about missing data

While allowing flexibility in specifying reference areas, data from the ACS are masked for sparsely populated places and may have too many missing values to return ADIs in some cases. 

Also, know when to use the ACS1, ACS3, or ACS5. See https://www.census.gov/programs-surveys/acs/guidance/estimates.html.
