## sociome 1.2.0

* Addressed the new errors in the package checks. 
* Change in behavior (households that have zero households are now excluded from ADI calculation)
* Updated README, tests, and manual files.
* Code improvements


## R CMD check results
There were no ERRORs or WARNINGs locally.

There continues to be a persistent NOTE on some platforms that the URL https://www.census.gov/programs-surveys/acs/guidance/estimates.html in get_adi.Rd may be invalid, but it is not.

There is an ERROR on win-builder that the package uses an unsuitable version of tidyr, but the DESCRIPTION file specifies tidyr (>= 1.0.0) in the Imports and tidyr 1.0.0 is indeed on CRAN. This update is primarily in response to the updating of tidyr to 1.0.0, so I suspect this is a spurious error.
