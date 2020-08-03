## sociome 1.4.2

Since examples wrapped in `\donttest{}` are now run during CRAN checks, all instances were changed to `\dontrun{}`. This is neccesary because the examples require a US Census API key, and they take a long time to run even if a valid key exists in the testing environment.

## R CMD check results
There were no ERRORs or WARNINGs.

There continues to be a persistent NOTE on some platforms that the URL https://www.census.gov/programs-surveys/acs/guidance/estimates.html in get_adi.Rd may be invalid, but it is not.
