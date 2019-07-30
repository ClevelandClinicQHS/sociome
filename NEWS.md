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