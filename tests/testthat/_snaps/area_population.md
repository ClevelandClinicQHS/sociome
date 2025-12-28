# area population functions work

    Code
      lon_lat_from_area("39035")
    Output
      [1] -81.67745  41.44841

---

    Code
      lon_lat_from_area(state = "oh", county = "cuyahoga")
    Output
      [1] -81.67745  41.44841

---

    Code
      areas_in_radius(geography = "state", center = lon_lat_from_area(state = "NY",
        county = "New York", year = 2010), year = 2010, radius = 300, units = "km")
    Output
      # A tibble: 7 x 7
        STATEFP STNAME        POPULATION LATITUDE LONGITUDE geoid distance
        <chr>   <chr>              <int>    <dbl>     <dbl> <chr>    <dbl>
      1 34      New Jersey       8791894     40.4     -74.4 34        55.1
      2 36      New York        19378102     41.5     -74.6 36        97.3
      3 09      Connecticut      3574097     41.5     -72.9 09       122. 
      4 10      Delaware          897934     39.4     -75.6 10       208. 
      5 44      Rhode Island     1052567     41.8     -71.5 44       237. 
      6 42      Pennsylvania    12702379     40.5     -77.0 42       260. 
      7 25      Massachusetts    6547629     42.3     -71.4 25       273. 

---

    Code
      areas_in_radius("tract", center = c(-109.0452, 36.9991), year = 2010, radius = 24)
    Output
      # A tibble: 4 x 8
        STATEFP COUNTYFP TRACTCE POPULATION LATITUDE LONGITUDE geoid       distance
        <chr>   <chr>    <chr>        <int>    <dbl>     <dbl> <chr>          <dbl>
      1 49      037      942000        3882     37.2     -109. 49037942000     20.2
      2 35      045      942801        2859     36.8     -109. 35045942801     21.7
      3 04      001      942700        5628     36.8     -109. 04001942700     22.4
      4 08      083      941100        1588     37.2     -109. 08083941100     23.0

---

    Code
      closest_n_areas(geography = "county", center = lon_lat_from_area("15007", year = 2010),
      year = 2010, n = 10, units = NULL)
    Output
      # A tibble: 10 x 9
         STATEFP COUNTYFP COUNAME        STNAME     POPULATION LATITUDE LONGITUDE
         <chr>   <chr>    <chr>          <chr>           <int>    <dbl>     <dbl>
       1 15      007      Kauai          Hawaii          67091     22.0     -159.
       2 15      003      Honolulu       Hawaii         953207     21.4     -158.
       3 15      005      Kalawao        Hawaii             90     21.2     -157.
       4 15      009      Maui           Hawaii         154834     20.9     -156.
       5 15      001      Hawaii         Hawaii         185079     19.7     -155.
       6 02      016      Aleutians West Alaska           5561     54.0     -168.
       7 02      013      Aleutians East Alaska           3141     54.9     -163.
       8 06      045      Mendocino      California      87841     39.3     -123.
       9 06      023      Humboldt       California     134623     40.8     -124.
      10 06      097      Sonoma         California     483878     38.4     -123.
         geoid distance
         <chr>    <dbl>
       1 15007       0 
       2 15003  173701.
       3 15005  271092.
       4 15009  331387.
       5 15001  492555.
       6 02016 3630113.
       7 02013 3657014.
       8 06045 3912409.
       9 06023 3912444.
      10 06097 3939071.

---

    Code
      closest_population(geography = "block group", center = lon_lat_from_area(state = "FL",
        year = 2010), year = 2010, population = 2000, units = "barleycorns")
    Output
      # A tibble: 2 x 9
        STATEFP COUNTYFP TRACTCE BLKGRPCE POPULATION LATITUDE LONGITUDE geoid       
        <chr>   <chr>    <chr>   <chr>         <int>    <dbl>     <dbl> <chr>       
      1 12      105      015405  1              1364     27.8     -81.6 121050154051
      2 12      105      015302  2              1600     27.8     -81.7 121050153022
        distance
           <dbl>
      1  426754.
      2  458559.

