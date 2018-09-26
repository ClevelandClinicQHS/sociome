## Test environments
* Platform: x86_64-pc-linux-gnu (64-bit)
  Running under: CentOS Linux 7 (Core)
  R 3.5.0
* Platform: x86_64-w64-mingw32 (64-bit)
  R Under development (unstable) (2018-08-22 r75177)
  using session charset: ISO8859-1
  
* New submission.


## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs. 



devtools::build_win() thinks one of the URLs in my documentation might be invalid, but it works for me on multiple platforms:

"Found the following (possibly) invalid URLs:
  URL: https://www.census.gov/programs-surveys/acs/guidance/estimates.html.
    From: man/get_adi.Rd
    Status: Error
    Message: libcurl error code 35:
      	error:14077410:SSL routines:SSL23_GET_SERVER_HELLO:sslv3 alert handshake failure"