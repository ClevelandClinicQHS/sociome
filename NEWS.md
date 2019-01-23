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