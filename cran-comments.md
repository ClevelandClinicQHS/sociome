
## Resubmission
This is a resubmission. In this version I have:

* Changed the LICENSE file so that it has the standard two lines.

* Added the National Institutes of Health/National Institute on Aging as authors in the DESCRIPTION file.

* Added more features and tests and changed the default of an important argument in the get_adi() function.


## Test environments
* Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: CentOS Linux 7 (Core)
  R version 3.5.0 (2018-04-23)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Platform: x86_64-w64-mingw32 (64-bit)
  R Under development (unstable) (2019-06-21 r76731)
  using session charset: ISO8859-1
  
* New submission.


## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 

Rhub thinks one of the URLs in my documentation might be invalid, but it works for me on multiple platforms:

"Found the following (possibly) invalid URLs:
  URL: https://www.census.gov/programs-surveys/acs/guidance/estimates.html.
    From: man/get_adi.Rd
    Status: Error
    Message: libcurl error code 35:
      	error:14077410:SSL routines:SSL23_GET_SERVER_HELLO:sslv3 alert handshake failure"