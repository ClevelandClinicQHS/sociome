# sociome (development version)

- Completed support of 2020 decennial census.
- Moved tibble from Suggests to Imports.
- Removed `seed` argument from `get_adi()`, `calculate_adi()`, and `synthetic_population()`. Just call `set.seed()` beforehand if you want.
- Added the helper objects `state_geoids` and `dataset_year_geography_availability`.
- Added `evaluator` argument to `get_adi()`, enabling the user to customize how the function retries calls to the census API.
- Made `synthetic_population()` and `get_geoids()` work with all `dataset` options (decennial census, acs1, acs3, and acs5).
- Added census block compatibility to `synthetic_population()` and `get_geoids()` with `dataset = "decennial"`.
- Added ZCTA compatibility to `get_geoids()`. You now have to exactly specify `dataset = "decennial"` with the exact decennial census year if you want these data.
- Fixed bug in the filtering of ZCTAs reference areas: before, the GEOIDs of ZCTAS were the ZCTA itself preceded by two digits. sociome now anticipates this and looks only at the last five digits.
- Added default argument to `geography` argument in `get_adi()`, `get_geoids()`, and `synthetic_population()`
- Changed default `year` argument in `get_geoids()` to 2020.
- Improved documentation and added many unit tests.

# sociome 2.2.5
- Fixed typo that prevented geometry from accompanying `get_adi()` results.
- Added escape hatches for examples and tests of functions requiring `USpopcenters` and `geosphere`.
- Updated email address of maintainer.

# sociome 2.2.1
- Added `synthetic_population()`
- Fixed `geography = "zcta"` behavior so that it will honor non-`NULL` values of `state`.
- Fixed incorrect message that only 1 or 2 calls to `tidycensus` functions were going to occur when in fact many might occur.
- Removed `shift_geo` as a formal argument from all functions. The user can still pass this via `...`.
- Removed `censusapi` from `Imports`

# sociome 2.1.0

- 2000 decennial ADI has been added back because the US census has restored the data to its API. 1990 ADI is still unavailable.
- Added functions `areas_in_radius()`, `closest_n_areas()`, `closest_population()`, `lon_lat_from_area()`, and `append_dissimilarities()`
- Fixed 2015 and 2016 block group pulls

# sociome 2.0.0

**Note that `get_adi()` will not work with 1990 nor 2000 Decennial Census data until the US Census restores the availability of those data.**

## Major updates
- The 3-factor split of ADI (i.e., the "Berg indices," or "ADI-3") have been incorporated and are now automatically included in `get_adi()` and `calculate_adi()` output.
- There is no longer a default for the `year` argument. This will break old code that relied on the default of `year = 2017`.

## Minor updates
- Updated README and documentation
- Lots of comments added
- Minor code fixes for clarity and greater ease in debugging
- Updated versions of Imports and Suggests

## Bug fixes
- The `seed` argument had been essentially ignored up until now, since `seed = 500` was hard-coded into the call to `mice::mice()` within `sociome::calculate_adi()`. User-specified `seed` values in `calculate_adi()` and `sociome::get_adi()` are now properly passed to `mice()`. Users desiring to recover old results should specify `seed = 500`.

# sociome 1.4.2

## Major updates
- `get_adi()` now pulls the correct variable for "civilian females age 16+ in white-collar occupations" for pre-2010 ACS estimates (C24010_040 instead of C24010_039).

## Minor updates
- Accommodations for new CRAN rules on referencing help pages in other packages.
- Since examples wrapped in `\donttest{}` are now run during CRAN checks, all instances were changed to `\dontrun{}`.

# sociome 1.4.1

## Major updates
- Fixed bug wherein `get_adi(dataset = "decennial", year = 2010)` calls did not work unless `geography = "tract"`

# sociome 1.4.0

## Major updates
- Fixed bug wherein `get_adi(dataset = "decennial", geography = "tract", year = 2010)` returned [incorrect] results for all tracts in all specified states, irrespective of what was passed to the `geoid` or `county` arguments.

## Minor updates
- Added `seed` argument to `get_adi()` and `calculate_adi()`.
- Code improvements
- Simplified testing

# sociome 1.3.2

## Major updates
- Made `tidycensus` 0.9.2 the minimum version, now that 0.9.6 is on CRAN. Both those versions are compatible, but 0.9.5 is not.

# sociome 1.3.1

## Major updates
- Currently, the latest CRAN version of `tidycensus`, 0.9.5, is incompatible with `sociome` due to a `tidycensus` bug. This bug is fixed in version 0.9.6, which is available on Github, but in order to accommodate new users of `sociome`, the older CRAN version, 0.9.2, will be required instead. This will be changed once the latest version of `tidycensus` becomes available on CRAN.

## Minor updates

- Removed reference to now-nonexistent `tigris-package` help page to avoid CRAN warning.

# sociome 1.3.0

## Major updates:
- Makes 2015 and 2016 block group calls use median household income (B19013_001) instead of median family income (B19113_001), which is unavailable at that level of geography during those years.

## Minor updates:
- Fixed tract-level 2010 decennial calls so that they go county-by-county for the ACS-data-gathering portions
- Made it so `get_adi(dataset = "decennial", state = NULL)` would work.

# sociome 1.2.0

## Major updates:
- Adapted to tidyr's breaking changes

## Minor updates:
- Added progress messages for the get_adi() calls that require many calls to tidycensus functions.
- Code improvements.

# sociome 1.1.0

## Major updates:
- Households that have zero households are now excluded from ADI calculation


## Minor updates:
- Updated README and documentation
- Added more tests


# sociome 1.0.2

## Minor updates:

- Fixed bug with 1990 decennial calls.
- Added `install.packages("sociome")` to README's installation instructions.
- Added CRAN version number and download counter badges to README.

# sociome 1.0.0

## Major updates:
- Made the `geometry` argument `FALSE` by default.
- Overhauled internal code to accommodate `dataset = "decennial"`.
- Added tests.

## Minor updates:
- Documentation and README improvements.

# sociome 0.7.0

## Major updates:
- Added "try again" feature and improved calls to tidycensus functions.
- Fixed ACS variable selection for get_adi().

## Minor updates:
- Made calculate_adi() an exported function again.
- Added warning() that `dataset = "decennial"` is under development.

# sociome 0.5.1.9000
- Made calls to tidycensus functions always broken up by state to accommodate its inconsistent behavior depending on geography and year.

# sociome 0.5.0

## Major updates:
- ZCTAs now supported, with the addition of `zcta` argument.

## Minor updates:
- Better code efficiency
- `checkmate` package no longer an Import.
- Fixed bug in which `cache_tables` argument had been ignored

# sociome 0.4.0

## Major updates:
- Created the function get_geoids(), which allows users to quickly create a table of state, county, tract, block group, and block GEOIDs.
- Made calculate_adi() an internal function (no longer exported)
- Made tidycensus 0.9.0 now required (added to Imports)

## Minor updates:

- Updated README.md.
- Bug fixes.

# sociome 0.3.4

## Major updates:
- Now using psych::principal() instead of psych::fa() under the hood.
- User is warned if attempting to use get_adi() to calculate ADI values for under 30 locations.

## Minor updates:
- README.md has been updated to reflect the above and has generally been refreshed.

# sociome 0.3.3

## Major Updates:
Successfully produces area deprivation indices (ADIs) for different US census designations based on user specifications.
