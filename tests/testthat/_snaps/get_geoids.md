# get_geoids()_works

    Code
      get_geoids(geography = "state", state = c("OH", "IN"), year = 2005, geometry = TRUE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 2 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -88.0585 ymin: 37.77174 xmax: -80.51869 ymax: 41.97752
      Geodetic CRS:  +proj=longlat +datum=NAD83 +no_defs
      # A tibble: 2 x 4
        GEOID NAME    census_2000_pop
      * <chr> <chr>             <dbl>
      1 18    Indiana         6080485
      2 39    Ohio           11353140
                                                                              geometry
      *                                                             <MULTIPOLYGON [°]>
      1 (((-85.99141 41.75995, -85.99592 41.76, -85.9973 41.76002, -86.00111 41.76006~
      2 (((-82.70731 41.61961, -82.7188 41.61963, -82.72355 41.61506, -82.73459 41.60~

---

    Code
      get_geoids(geography = "state", state = c("OH", "IN"), year = 2012, geometry = TRUE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 2 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -88.0585 ymin: 37.77174 xmax: -80.51869 ymax: 41.97752
      Geodetic CRS:  NAD83
      # A tibble: 2 x 4
        GEOID NAME    census_2010_pop
      * <chr> <chr>             <dbl>
      1 39    Ohio           11536504
      2 18    Indiana         6483802
                                                                              geometry
      *                                                             <MULTIPOLYGON [°]>
      1 (((-82.81349 41.72347, -82.81049 41.72052, -82.80887 41.70833, -82.81613 41.7~
      2 (((-88.02803 37.79922, -88.02938 37.8036, -88.0391 37.80579, -88.04594 37.807~

---

    Code
      get_geoids(geography = "state", state = c("OH", "IN"), year = 2025, geometry = TRUE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Note: 2020 decennial Census data use differential privacy, a technique that
      introduces errors into data to preserve respondent confidentiality.
      i Small counts should be interpreted with caution.
      i See https://www.census.gov/library/fact-sheets/2021/protecting-the-confidentiality-of-the-2020-census-redistricting-data.html for additional guidance.
      This message is displayed once per session.
    Output
      Simple feature collection with 2 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -88.09789 ymin: 37.77174 xmax: -80.51869 ymax: 41.97716
      Geodetic CRS:  NAD83
      # A tibble: 2 x 4
        GEOID NAME    census_2020_pop
      * <chr> <chr>             <dbl>
      1 18    Indiana         6785528
      2 39    Ohio           11799448
                                                                              geometry
      *                                                             <MULTIPOLYGON [°]>
      1 (((-88.09776 37.90403, -88.09448 37.90596, -88.08624 37.90571, -88.07145 37.8~
      2 (((-82.73447 41.60351, -82.72425 41.60897, -82.72226 41.61299, -82.72264 41.6~

---

    Code
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2005, geometry = TRUE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 4 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.1199 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  +proj=longlat +datum=NAD83 +no_defs
      # A tibble: 4 x 4
        GEOID NAME                                       census_2000_pop
      * <chr> <chr>                                                <dbl>
      1 10001 Kent County, Delaware                               126697
      2 10003 New Castle County, Delaware                         500265
      3 10005 Sussex County, Delaware                             156638
      4 11001 District of Columbia, District of Columbia          572059
                                                                              geometry
      *                                                             <MULTIPOLYGON [°]>
      1 (((-75.62149 39.30887, -75.62287 39.30808, -75.62386 39.30635, -75.62806 39.3~
      2 (((-75.56582 39.59061, -75.57962 39.59866, -75.57872 39.5915, -75.57627 39.58~
      3 (((-75.04906 38.45635, -75.04931 38.46747, -75.04975 38.48639, -75.05158 38.5~
      4 (((-77.0363 38.99171, -77.0403 38.9946, -77.041 38.99511, -77.04146 38.99476,~

---

    Code
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2012, geometry = TRUE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 4 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.1199 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 4 x 4
        GEOID NAME                                       census_2010_pop
      * <chr> <chr>                                                <dbl>
      1 10001 Kent County, Delaware                               162310
      2 10003 New Castle County, Delaware                         538479
      3 10005 Sussex County, Delaware                             197145
      4 11001 District of Columbia, District of Columbia          601723
                                                                              geometry
      *                                                             <MULTIPOLYGON [°]>
      1 (((-75.7601 39.29682, -75.7149 39.29937, -75.68486 39.2969, -75.68006 39.2924~
      2 (((-75.5655 39.58797, -75.56493 39.58325, -75.56875 39.5849, -75.57405 39.587~
      3 (((-75.69878 38.52203, -75.70018 38.54272, -75.70156 38.56073, -75.70157 38.5~
      4 (((-76.95772 38.9308, -76.94192 38.91859, -76.9351 38.91331, -76.93187 38.910~

---

    Code
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2025, geometry = TRUE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
    Output
      Simple feature collection with 4 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.11976 ymin: 38.4512 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 4 x 4
        GEOID NAME                                       census_2020_pop
      * <chr> <chr>                                                <dbl>
      1 10001 Kent County, Delaware                               181851
      2 11001 District of Columbia, District of Columbia          689545
      3 10003 New Castle County, Delaware                         570719
      4 10005 Sussex County, Delaware                             237378
                                                                              geometry
      *                                                             <MULTIPOLYGON [°]>
      1 (((-75.7601 39.29682, -75.75 39.29747, -75.74605 39.29753, -75.7149 39.29937,~
      2 (((-77.11976 38.93434, -77.11253 38.94006, -77.11248 38.9401, -77.1045 38.946~
      3 (((-75.56752 39.5102, -75.56477 39.51595, -75.56258 39.51911, -75.56132 39.51~
      4 (((-75.7226 38.82986, -75.61542 38.83362, -75.58242 38.83464, -75.5553 38.835~

---

    Code
      get_geoids(geography = "tract", state = c("DE", "DC"), year = 2005, geometry = TRUE)
    Message
      
      2 call(s) to tidycensus beginning.
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 385 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.1199 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  +proj=longlat +datum=NAD83 +no_defs
      # A tibble: 385 x 4
         GEOID       NAME                                       census_2000_pop
       * <chr>       <chr>                                                <dbl>
       1 10001040100 Census Tract 401, Kent County, Delaware               5337
       2 10001040201 Census Tract 402.01, Kent County, Delaware            3477
       3 10001040202 Census Tract 402.02, Kent County, Delaware            5189
       4 10001040203 Census Tract 402.03, Kent County, Delaware            3069
       5 10001040400 Census Tract 404, Kent County, Delaware               1156
       6 10001040500 Census Tract 405, Kent County, Delaware               8218
       7 10001040600 Census Tract 406, Kent County, Delaware               2380
       8 10001040700 Census Tract 407, Kent County, Delaware               4470
       9 10001040800 Census Tract 408, Kent County, Delaware               2770
      10 10001040900 Census Tract 409, Kent County, Delaware               2407
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.75819 39.27335, -75.75695 39.25813, -75.75595 39.24596, -75.75258 39.~
       2 (((-75.62287 39.30808, -75.62386 39.30635, -75.62806 39.30426, -75.63402 39.~
       3 (((-75.6119 39.29685, -75.61587 39.29584, -75.61669 39.29559, -75.6201 39.29~
       4 (((-75.51515 39.3158, -75.51341 39.32221, -75.51323 39.32621, -75.51803 39.3~
       5 (((-75.54877 39.35119, -75.54146 39.34149, -75.52784 39.3329, -75.51803 39.3~
       6 (((-75.52223 39.24554, -75.52459 39.24607, -75.52508 39.24731, -75.52414 39.~
       7 (((-75.56641 39.18758, -75.56346 39.18474, -75.55715 39.17916, -75.55538 39.~
       8 (((-75.55538 39.17759, -75.55715 39.17916, -75.56346 39.18474, -75.56641 39.~
       9 (((-75.55278 39.17692, -75.55377 39.17616, -75.55364 39.17607, -75.54821 39.~
      10 (((-75.52704 39.17838, -75.52962 39.18085, -75.52998 39.17419, -75.53004 39.~
      # i 375 more rows

---

    Code
      get_geoids(geography = "tract", state = c("DE", "DC"), year = 2012, geometry = TRUE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 397 features and 3 fields
      Geometry type: GEOMETRY
      Dimension:     XY
      Bounding box:  xmin: -77.1199 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 397 x 4
         GEOID       NAME                                       census_2010_pop
       * <chr>       <chr>                                                <dbl>
       1 10001042800 Census Tract 428, Kent County, Delaware               7085
       2 10001042900 Census Tract 429, Kent County, Delaware               4794
       3 10001043100 Census Tract 431, Kent County, Delaware               2630
       4 10001043300 Census Tract 433, Kent County, Delaware               6131
       5 10001043400 Census Tract 434, Kent County, Delaware               4146
       6 10001990000 Census Tract 9900, Kent County, Delaware                 0
       7 10001040100 Census Tract 401, Kent County, Delaware               6541
       8 10001040501 Census Tract 405.01, Kent County, Delaware            4923
       9 10001041300 Census Tract 413, Kent County, Delaware               2068
      10 10001041702 Census Tract 417.02, Kent County, Delaware            4216
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.73286 38.95737, -75.73845 39.02751, -75.73863 39.02971, -75.73559 39.~
       2 (((-75.48935 38.90583, -75.48939 38.90567, -75.48949 38.9055, -75.48909 38.9~
       3 (((-75.724 38.84672, -75.72406 38.84778, -75.72557 38.86815, -75.72583 38.86~
       4 (((-75.52838 39.16143, -75.52827 39.16111, -75.53776 39.15901, -75.54285 39.~
       5 (((-75.48949 38.9055, -75.48939 38.90567, -75.48935 38.90583, -75.49218 38.9~
       6 (((-75.40328 39.25522, -75.40622 39.25464, -75.40534 39.2574, -75.40647 39.2~
       7 (((-75.7601 39.29682, -75.7149 39.29937, -75.68486 39.2969, -75.68006 39.292~
       8 (((-75.56513 39.20658, -75.56921 39.20945, -75.57085 39.2113, -75.57106 39.2~
       9 (((-75.52762 39.16126, -75.52359 39.16216, -75.52296 39.1623, -75.51528 39.1~
      10 (((-75.54567 39.09522, -75.54595 39.09522, -75.54287 39.10185, -75.54106 39.~
      # i 387 more rows

---

    Code
      get_geoids(geography = "tract", state = c("DE", "DC"), year = 2025, geometry = TRUE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
    Output
      Simple feature collection with 468 features and 3 fields (with 3 geometries empty)
      Geometry type: GEOMETRY
      Dimension:     XY
      Bounding box:  xmin: -77.11976 ymin: 38.4512 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 468 x 4
         GEOID       NAME                                       census_2020_pop
       * <chr>       <chr>                                                <dbl>
       1 10001040201 Census Tract 402.01, Kent County, Delaware            5446
       2 10001041101 Census Tract 411.01, Kent County, Delaware            2816
       3 10001043300 Census Tract 433, Kent County, Delaware               6553
       4 10001041600 Census Tract 416, Kent County, Delaware               2107
       5 10001040900 Census Tract 409, Kent County, Delaware               2374
       6 10001042203 Census Tract 422.03, Kent County, Delaware            5705
       7 10001042100 Census Tract 421, Kent County, Delaware               3997
       8 10001041900 Census Tract 419, Kent County, Delaware               5289
       9 10001043400 Census Tract 434, Kent County, Delaware               5648
      10 10001040203 Census Tract 402.03, Kent County, Delaware            5182
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.66581 39.29057, -75.66048 39.29167, -75.65446 39.29118, -75.65295 39.~
       2 (((-75.49858 39.115, -75.49508 39.116, -75.49352 39.11844, -75.49332 39.1240~
       3 (((-75.5712 39.19105, -75.5682 39.19298, -75.56761 39.19326, -75.56731 39.19~
       4 (((-75.53757 39.11309, -75.53657 39.11521, -75.53647 39.11543, -75.53315 39.~
       5 (((-75.53129 39.1709, -75.52996 39.17295, -75.52992 39.17403, -75.52987 39.1~
       6 (((-75.51742 39.09485, -75.51617 39.09802, -75.51587 39.10033, -75.51586 39.~
       7 (((-75.64906 39.04903, -75.64111 39.05261, -75.64036 39.05313, -75.6395 39.0~
       8 (((-75.75259 39.19943, -75.74623 39.20031, -75.74187 39.18909, -75.74119 39.~
       9 (((-75.52892 39.01019, -75.52536 39.01279, -75.52427 39.01462, -75.52111 39.~
      10 (((-75.60939 39.3088, -75.6066 39.30809, -75.60384 39.31118, -75.60345 39.30~
      # i 458 more rows

---

    Code
      get_geoids(geography = "block group", state = c("DE", "DC"), year = 2005,
      geometry = TRUE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 935 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.1199 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  +proj=longlat +datum=NAD83 +no_defs
      # A tibble: 935 x 4
         GEOID        NAME                                                     
       * <chr>        <chr>                                                    
       1 100010401001 Block Group 1, Census Tract 401, Kent County, Delaware   
       2 100010401002 Block Group 2, Census Tract 401, Kent County, Delaware   
       3 100010401003 Block Group 3, Census Tract 401, Kent County, Delaware   
       4 100010402011 Block Group 1, Census Tract 402.01, Kent County, Delaware
       5 100010402012 Block Group 2, Census Tract 402.01, Kent County, Delaware
       6 100010402021 Block Group 1, Census Tract 402.02, Kent County, Delaware
       7 100010402031 Block Group 1, Census Tract 402.03, Kent County, Delaware
       8 100010402032 Block Group 2, Census Tract 402.03, Kent County, Delaware
       9 100010404001 Block Group 1, Census Tract 404, Kent County, Delaware   
      10 100010405001 Block Group 1, Census Tract 405, Kent County, Delaware   
         census_2000_pop
       *           <dbl>
       1            1948
       2            1695
       3            1694
       4            1621
       5            1856
       6            5189
       7            1104
       8            1965
       9            1156
      10            1351
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.75819 39.27335, -75.75695 39.25813, -75.738 39.26226, -75.73437 39.25~
       2 (((-75.75595 39.24596, -75.75258 39.19926, -75.74634 39.20029, -75.74001 39.~
       3 (((-75.65823 39.20751, -75.65976 39.20873, -75.67047 39.22294, -75.67297 39.~
       4 (((-75.62287 39.30808, -75.62386 39.30635, -75.62806 39.30426, -75.63402 39.~
       5 (((-75.60979 39.30877, -75.61144 39.30916, -75.61423 39.30829, -75.61653 39.~
       6 (((-75.6119 39.29685, -75.61587 39.29584, -75.61669 39.29559, -75.6201 39.29~
       7 (((-75.51515 39.3158, -75.51341 39.32221, -75.51323 39.32621, -75.51803 39.3~
       8 (((-75.59965 39.30461, -75.60389 39.30257, -75.59996 39.29711, -75.59779 39.~
       9 (((-75.54877 39.35119, -75.54146 39.34149, -75.52784 39.3329, -75.51803 39.3~
      10 (((-75.52223 39.24554, -75.52459 39.24607, -75.52508 39.24731, -75.52414 39.~
      # i 925 more rows

---

    Code
      get_geoids(geography = "block group", state = c("DE", "DC"), year = 2012,
      geometry = TRUE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 1024 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.1199 ymin: 38.45101 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 1,024 x 4
         GEOID        NAME                                                     
       * <chr>        <chr>                                                    
       1 100010401001 Block Group 1, Census Tract 401, Kent County, Delaware   
       2 100010401002 Block Group 2, Census Tract 401, Kent County, Delaware   
       3 100010402012 Block Group 2, Census Tract 402.01, Kent County, Delaware
       4 100010405012 Block Group 2, Census Tract 405.01, Kent County, Delaware
       5 100010407002 Block Group 2, Census Tract 407, Kent County, Delaware   
       6 100010412001 Block Group 1, Census Tract 412, Kent County, Delaware   
       7 100010417021 Block Group 1, Census Tract 417.02, Kent County, Delaware
       8 100010418013 Block Group 3, Census Tract 418.01, Kent County, Delaware
       9 100010422011 Block Group 1, Census Tract 422.01, Kent County, Delaware
      10 100010425002 Block Group 2, Census Tract 425, Kent County, Delaware   
         census_2010_pop
       *           <dbl>
       1            2637
       2            2174
       3            2375
       4            2574
       5            1449
       6            1809
       7            2378
       8            3014
       9            4261
      10            1988
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.75847 39.27683, -75.7601 39.29682, -75.7149 39.29937, -75.68486 39.29~
       2 (((-75.74623 39.20031, -75.75259 39.19943, -75.75595 39.24596, -75.75694 39.~
       3 (((-75.62434 39.29377, -75.62401 39.29403, -75.62309 39.29436, -75.6226 39.2~
       4 (((-75.51712 39.16811, -75.51915 39.17101, -75.52643 39.17801, -75.52956 39.~
       5 (((-75.55564 39.16734, -75.56105 39.16613, -75.56227 39.16667, -75.55673 39.~
       6 (((-75.51577 39.12758, -75.51617 39.12742, -75.51785 39.12697, -75.52082 39.~
       7 (((-75.52501 39.10576, -75.53204 39.11157, -75.53494 39.11294, -75.53499 39.~
       8 (((-75.58396 39.15594, -75.59153 39.15572, -75.61283 39.1557, -75.60558 39.1~
       9 (((-75.51587 39.10033, -75.51586 39.10034, -75.50162 39.10991, -75.49961 39.~
      10 (((-75.42991 38.9333, -75.42879 38.92487, -75.42857 38.91676, -75.42824 38.9~
      # i 1,014 more rows

---

    Code
      get_geoids(geography = "block group", state = c("DE", "DC"), year = 2025,
      geometry = TRUE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
      Getting data from the 2020 decennial Census
      Using the PL 94-171 Redistricting Data Summary File
    Output
      Simple feature collection with 1277 features and 3 fields (with 3 geometries empty)
      Geometry type: GEOMETRY
      Dimension:     XY
      Bounding box:  xmin: -77.11976 ymin: 38.4512 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 1,277 x 4
         GEOID        NAME                                                     
       * <chr>        <chr>                                                    
       1 100010412001 Block Group 1, Census Tract 412, Kent County, Delaware   
       2 100010422061 Block Group 1, Census Tract 422.06, Kent County, Delaware
       3 100010434002 Block Group 2, Census Tract 434, Kent County, Delaware   
       4 100010432023 Block Group 3, Census Tract 432.02, Kent County, Delaware
       5 100010409002 Block Group 2, Census Tract 409, Kent County, Delaware   
       6 100010418043 Block Group 3, Census Tract 418.04, Kent County, Delaware
       7 100010422062 Block Group 2, Census Tract 422.06, Kent County, Delaware
       8 100010402033 Block Group 3, Census Tract 402.03, Kent County, Delaware
       9 100010428022 Block Group 2, Census Tract 428.02, Kent County, Delaware
      10 100010422082 Block Group 2, Census Tract 422.08, Kent County, Delaware
         census_2020_pop
       *           <dbl>
       1            1861
       2            1295
       3             685
       4            1066
       5            1063
       6             985
       7            1527
       8            2542
       9            2677
      10            2141
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.51785 39.12697, -75.51598 39.12747, -75.51524 39.12791, -75.51448 39.~
       2 (((-75.55582 39.04252, -75.55319 39.04299, -75.54966 39.04251, -75.54817 39.~
       3 (((-75.49268 38.92271, -75.49085 38.9236, -75.49042 38.92399, -75.49141 38.9~
       4 (((-75.46158 39.06739, -75.45864 39.08078, -75.45729 39.08562, -75.45481 39.~
       5 (((-75.52956 39.181, -75.52643 39.17801, -75.51915 39.17101, -75.51811 39.16~
       6 (((-75.60856 39.16073, -75.60524 39.16461, -75.60423 39.16498, -75.60135 39.~
       7 (((-75.55195 39.03752, -75.54574 39.03918, -75.54154 39.03953, -75.53164 39.~
       8 (((-75.60939 39.3088, -75.6066 39.30809, -75.60384 39.31118, -75.60345 39.30~
       9 (((-75.57912 38.98595, -75.57748 38.99925, -75.57739 38.99983, -75.57687 39.~
      10 (((-75.51781 39.05206, -75.51379 39.05203, -75.5067 39.05299, -75.50114 39.0~
      # i 1,267 more rows

