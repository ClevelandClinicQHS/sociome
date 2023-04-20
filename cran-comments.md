
## Resubmission

* Added new functions `areas_in_radius()`, `closest_n_areas()`, `closest_population()`, `append_dissimilarities()`, and `synthetic_population()`
* Updated minimum required version of R to 3.6.0 since we now use `asplit()`
* Updated README and code and documentation


## Test environments:

* CentOS Linux 8, R 4.0.3
* CentOS Linux 7, R 4.0.0
* Fedora Linux, R-devel
* Ubuntu Linux 20.04.1 LTS, R 4.1.1
* Windows Server 2008 R2 SP1, R-devel
* winbuilder devel


## R CMD check results
There were no ERRORs or WARNINGs.

There continues to be a persistent NOTE on some platforms that the URL https://www.census.gov/programs-surveys/acs/guidance/estimates.html in get_adi.Rd may be invalid, but it is not.
