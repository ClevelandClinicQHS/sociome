
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

There was one NOTE from the check on Windows Server 2022, R-devel, 64 bit on Rhub:

checking for detritus in the temp directory … NOTE ‘lastMiKTeXException’
Apparently, this is a known issue with Rhub and does not suggest a problem with the package.
