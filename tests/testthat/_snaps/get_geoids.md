# get_geoids()_works

    Code
      get_geoids(geography = "state", state = c("OH", "IN"), year = 2000, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "state", state = c("OH", "IN"), year = 2010, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "state", state = c("OH", "IN"), year = 2020, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
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
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2000, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2010, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2020, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
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
      get_geoids(geography = "tract", state = c("DE", "DC"), year = 2000, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "tract", state = c("DE", "DC"), year = 2010, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "tract", state = c("DE", "DC"), year = 2020, dataset = "decennial",
      geometry = TRUE, progress_bar = FALSE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
    Output
      Simple feature collection with 468 features and 3 fields (with 3 geometries empty)
      Geometry type: GEOMETRY
      Dimension:     XY
      Bounding box:  xmin: -77.11976 ymin: 38.4512 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 468 x 4
         GEOID       NAME                                             census_2020_pop
       * <chr>       <chr>                                                      <dbl>
       1 10003010300 Census Tract 103; New Castle County; Delaware               4106
       2 10003011206 Census Tract 112.06; New Castle County; Delaware            4319
       3 10003013903 Census Tract 139.03; New Castle County; Delaware            4820
       4 10003015600 Census Tract 156; New Castle County; Delaware               2343
       5 10003013000 Census Tract 130; New Castle County; Delaware               1927
       6 10003000300 Census Tract 3; New Castle County; Delaware                 3012
       7 10003000601 Census Tract 6.01; New Castle County; Delaware              2773
       8 10003002200 Census Tract 22; New Castle County; Delaware                2874
       9 10003014906 Census Tract 149.06; New Castle County; Delaware            5339
      10 10003016309 Census Tract 163.09; New Castle County; Delaware            4125
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.48088 39.80435, -75.4763 39.80829, -75.47301 39.80608, -75.47306 39.8~
       2 (((-75.53582 39.81255, -75.53506 39.81905, -75.53451 39.82088, -75.53452 39.~
       3 (((-75.70552 39.63554, -75.70252 39.64525, -75.70296 39.64787, -75.69772 39.~
       4 (((-75.57671 39.69587, -75.56486 39.70905, -75.56453 39.70943, -75.56422 39.~
       5 (((-75.65017 39.72018, -75.64544 39.72187, -75.63896 39.72406, -75.63501 39.~
       6 (((-75.54275 39.76461, -75.53928 39.76604, -75.53669 39.76265, -75.53589 39.~
       7 (((-75.53454 39.75159, -75.53329 39.75279, -75.53143 39.75455, -75.53175 39.~
       8 (((-75.5669 39.74468, -75.5662 39.74573, -75.5658 39.74633, -75.56479 39.747~
       9 (((-75.6807 39.63759, -75.67705 39.63947, -75.67631 39.64224, -75.6751 39.64~
      10 (((-75.62532 39.64382, -75.62179 39.64866, -75.61935 39.65151, -75.61691 39.~
      # i 458 more rows

---

    Code
      get_geoids(geography = "block group", state = c("DE", "DC"), year = 2000,
      dataset = "decennial", geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "block group", state = c("DE", "DC"), year = 2010,
      dataset = "decennial", geometry = TRUE, progress_bar = FALSE)
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
      get_geoids(geography = "block group", state = c("DE", "DC"), year = 2020,
      dataset = "decennial", geometry = TRUE, progress_bar = FALSE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
    Output
      Simple feature collection with 1277 features and 3 fields (with 3 geometries empty)
      Geometry type: GEOMETRY
      Dimension:     XY
      Bounding box:  xmin: -77.11976 ymin: 38.4512 xmax: -75.04894 ymax: 39.83901
      Geodetic CRS:  NAD83
      # A tibble: 1,277 x 4
         GEOID        NAME                                                           
       * <chr>        <chr>                                                          
       1 100030027002 Block Group 2; Census Tract 27; New Castle County; Delaware    
       2 100030161001 Block Group 1; Census Tract 161; New Castle County; Delaware   
       3 100030136082 Block Group 2; Census Tract 136.08; New Castle County; Delaware
       4 100030027001 Block Group 1; Census Tract 27; New Castle County; Delaware    
       5 100030119001 Block Group 1; Census Tract 119; New Castle County; Delaware   
       6 100030112022 Block Group 2; Census Tract 112.02; New Castle County; Delaware
       7 100030117001 Block Group 1; Census Tract 117; New Castle County; Delaware   
       8 100030145021 Block Group 1; Census Tract 145.02; New Castle County; Delaware
       9 100030139032 Block Group 2; Census Tract 139.03; New Castle County; Delaware
      10 100030140003 Block Group 3; Census Tract 140; New Castle County; Delaware   
         census_2020_pop
       *           <dbl>
       1            1442
       2            1148
       3            1029
       4            1548
       5            1485
       6            1428
       7            1375
       8            2312
       9            1148
      10            1353
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-75.57307 39.735, -75.56912 39.73658, -75.56733 39.73392, -75.5662 39.734~
       2 (((-75.5747 39.67215, -75.57123 39.67112, -75.57029 39.67181, -75.5704 39.67~
       3 (((-75.69056 39.71093, -75.68804 39.71304, -75.68381 39.71275, -75.68159 39.~
       4 (((-75.57053 39.72928, -75.56431 39.73569, -75.5662 39.73422, -75.56733 39.7~
       5 (((-75.60437 39.75877, -75.5993 39.76376, -75.59883 39.76526, -75.59829 39.7~
       6 (((-75.54242 39.83489, -75.54006 39.83823, -75.53973 39.83822, -75.53935 39.~
       7 (((-75.56836 39.79873, -75.56504 39.80033, -75.5542 39.80545, -75.54887 39.8~
       8 (((-75.75351 39.67443, -75.75058 39.67433, -75.7448 39.67416, -75.74306 39.6~
       9 (((-75.6911 39.65036, -75.68882 39.64683, -75.68749 39.64555, -75.68697 39.6~
      10 (((-75.68863 39.68912, -75.68472 39.68825, -75.68264 39.6865, -75.68192 39.6~
      # i 1,267 more rows

---

    Code
      get_geoids(geography = "block", state = "HI", county = "Kalawao", year = 2000,
        dataset = "decennial", geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 44 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -157.0151 ymin: 21.12948 xmax: -156.8712 ymax: 21.27462
      Geodetic CRS:  NAD83
      # A tibble: 44 x 4
         GEOID          
       * <chr>          
       1 150050319001029
       2 150050319001013
       3 150050319001007
       4 150050319001003
       5 150050319001040
       6 150050319001025
       7 150050319001018
       8 150050319001036
       9 150050319001030
      10 150050319001034
         NAME                                                               
       * <chr>                                                              
       1 Block 1029, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       2 Block 1013, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       3 Block 1007, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       4 Block 1003, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       5 Block 1040, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       6 Block 1025, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       7 Block 1018, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       8 Block 1036, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       9 Block 1030, Block Group 1, Census Tract 319, Kalawao County, Hawaii
      10 Block 1034, Block Group 1, Census Tract 319, Kalawao County, Hawaii
         census_2000_pop
       *           <dbl>
       1               3
       2               2
       3               1
       4               0
       5               8
       6               1
       7               2
       8               4
       9               4
      10               2
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-156.9811 21.19003, -156.9811 21.18996, -156.9812 21.1894, -156.9813 21.1~
       2 (((-156.9793 21.19308, -156.9793 21.19285, -156.9794 21.1927, -156.9794 21.1~
       3 (((-156.966 21.16078, -156.9652 21.16046, -156.9641 21.15996, -156.9635 21.1~
       4 (((-156.9774 21.20467, -156.9772 21.20511, -156.9767 21.20613, -156.9765 21.~
       5 (((-156.9833 21.18806, -156.9832 21.18793, -156.983 21.18779, -156.983 21.18~
       6 (((-156.9833 21.19033, -156.9834 21.19031, -156.9835 21.19031, -156.9836 21.~
       7 (((-156.9832 21.19143, -156.9832 21.19134, -156.9833 21.19109, -156.9833 21.~
       8 (((-156.9821 21.18822, -156.9821 21.18824, -156.9821 21.18824, -156.982 21.1~
       9 (((-156.9809 21.18998, -156.9808 21.18995, -156.9806 21.18991, -156.9805 21.~
      10 (((-156.9808 21.18614, -156.9808 21.18627, -156.9808 21.18633, -156.9808 21.~
      # i 34 more rows

---

    Code
      get_geoids(geography = "block", state = "HI", county = "Kalawao", year = 2010,
        dataset = "decennial", geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 63 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -157.0151 ymin: 21.12948 xmax: -156.8712 ymax: 21.27462
      Geodetic CRS:  NAD83
      # A tibble: 63 x 4
         GEOID          
       * <chr>          
       1 150050319001032
       2 150050319001040
       3 150050319001013
       4 150050319001001
       5 150050319001050
       6 150050319001008
       7 150050319001027
       8 150050319001024
       9 150050319001056
      10 150050319001048
         NAME                                                               
       * <chr>                                                              
       1 Block 1032, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       2 Block 1040, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       3 Block 1013, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       4 Block 1001, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       5 Block 1050, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       6 Block 1008, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       7 Block 1027, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       8 Block 1024, Block Group 1, Census Tract 319, Kalawao County, Hawaii
       9 Block 1056, Block Group 1, Census Tract 319, Kalawao County, Hawaii
      10 Block 1048, Block Group 1, Census Tract 319, Kalawao County, Hawaii
         census_2010_pop
       *           <dbl>
       1               1
       2               0
       3               0
       4               0
       5               0
       6               0
       7              10
       8               0
       9               0
      10               2
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-156.9797 21.18618, -156.9797 21.18651, -156.9796 21.18734, -156.9796 21.~
       2 (((-156.9737 21.17376, -156.9736 21.17369, -156.9734 21.17357, -156.9732 21.~
       3 (((-156.9794 21.2047, -156.9789 21.20478, -156.9789 21.20467, -156.979 21.20~
       4 (((-156.9787 21.20482, -156.9789 21.20478, -156.9789 21.20481, -156.9788 21.~
       5 (((-156.96 21.18479, -156.9602 21.18468, -156.9603 21.18462, -156.961 21.184~
       6 (((-156.9792 21.20591, -156.9792 21.20594, -156.9793 21.20599, -156.9793 21.~
       7 (((-156.9797 21.18981, -156.9797 21.18984, -156.9797 21.18988, -156.9797 21.~
       8 (((-156.9783 21.20489, -156.9772 21.20511, -156.9774 21.20467, -156.9777 21.~
       9 (((-156.9833 21.18806, -156.9834 21.18816, -156.9835 21.18825, -156.9835 21.~
      10 (((-156.9832 21.19143, -156.983 21.19141, -156.9828 21.19139, -156.9828 21.1~
      # i 53 more rows

---

    Code
      get_geoids(geography = "block", state = "HI", county = "Kalawao", year = 2020,
        dataset = "decennial", geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
    Output
      Simple feature collection with 50 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -157.0151 ymin: 21.12948 xmax: -156.8712 ymax: 21.27462
      Geodetic CRS:  NAD83
      # A tibble: 50 x 4
         GEOID          
       * <chr>          
       1 150050319001001
       2 150059900000003
       3 150050319001038
       4 150050319001043
       5 150050319001012
       6 150059900000002
       7 150050319001013
       8 150050319001031
       9 150050319001008
      10 150050319001046
         NAME                                                                
       * <chr>                                                               
       1 Block 1001, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
       2 Block 0003, Block Group 0, Census Tract 9900, Kalawao County, Hawaii
       3 Block 1038, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
       4 Block 1043, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
       5 Block 1012, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
       6 Block 0002, Block Group 0, Census Tract 9900, Kalawao County, Hawaii
       7 Block 1013, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
       8 Block 1031, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
       9 Block 1008, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
      10 Block 1046, Block Group 1, Census Tract 319, Kalawao County, Hawaii 
         census_2020_pop
       *           <dbl>
       1               0
       2               0
       3               0
       4               0
       5              23
       6               0
       7               0
       8               4
       9               0
      10               0
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-156.9927 21.19615, -156.991 21.19865, -156.9896 21.20149, -156.9887 21.2~
       2 (((-157 21.19293, -157 21.19521, -156.996 21.19507, -156.9939 21.195, -156.9~
       3 (((-157.0146 21.18491, -157.0145 21.18784, -157.0145 21.18882, -157.0125 21.~
       4 (((-156.9593 21.17216, -156.9591 21.17295, -156.9589 21.1727, -156.959 21.17~
       5 (((-156.9832 21.19241, -156.9832 21.19252, -156.9831 21.19283, -156.9829 21.~
       6 (((-157.015 21.25083, -156.9825 21.25083, -156.95 21.25084, -156.95 21.2333,~
       7 (((-156.983 21.19037, -156.9829 21.19039, -156.9825 21.19044, -156.9823 21.1~
       8 (((-156.9954 21.17826, -156.9948 21.17866, -156.9946 21.17874, -156.9932 21.~
       9 (((-157 21.19084, -156.9993 21.19128, -156.9987 21.19159, -156.9969 21.19263~
      10 (((-156.9833 21.19017, -156.9833 21.19027, -156.9833 21.19033, -156.9832 21.~
      # i 40 more rows

---

    Code
      zcta2000
    Output
      Simple feature collection with 50087 features and 3 fields (with 7 geometries empty)
      Geometry type: POLYGON
      Dimension:     XY
      Bounding box:  xmin: -178.3421 ymin: 17.88481 xmax: -65.24442 ymax: 71.35256
      Geodetic CRS:  +proj=longlat +datum=NAD83 +no_defs
      # A tibble: 50,087 x 4
         GEOID NAME        census_2000_pop
       * <chr> <chr>                 <dbl>
       1 997XX ZCTA5 997XX             644
       2 99723 ZCTA5 99723            4572
       3 997HH ZCTA5 997HH               0
       4 99782 ZCTA5 99782             546
       5 997XX ZCTA5 997XX             644
       6 997XX ZCTA5 997XX             644
       7 997XX ZCTA5 997XX             644
       8 997XX ZCTA5 997XX             644
       9 997XX ZCTA5 997XX             644
      10 99791 ZCTA5 99791             228
                                                                              geometry
       *                                                                 <POLYGON [°]>
       1 ((-156.9249 71.23033, -156.7287 71.2304, -156.7328 71.21652, -156.734 71.214~
       2 ((-156.6725 71.32994, -156.6643 71.32519, -156.6565 71.31874, -156.6477 71.3~
       3 ((-154.1048 70.63819, -154.0968 70.65017, -154.0651 70.65417, -154.0715 70.6~
       4 ((-159.9278 70.68501, -159.9264 70.68479, -159.9189 70.68305, -159.826 70.66~
       5 ((-156.9249 71.23033, -156.7287 71.2304, -156.7328 71.21652, -156.734 71.214~
       6 ((-156.9249 71.23033, -156.7287 71.2304, -156.7328 71.21652, -156.734 71.214~
       7 ((-156.9249 71.23033, -156.7287 71.2304, -156.7328 71.21652, -156.734 71.214~
       8 ((-156.9249 71.23033, -156.7287 71.2304, -156.7328 71.21652, -156.734 71.214~
       9 ((-156.9249 71.23033, -156.7287 71.2304, -156.7328 71.21652, -156.734 71.214~
      10 ((-157.3003 70.54544, -157.3005 70.51797, -157.2559 70.51821, -157.2568 70.4~
      # i 50,077 more rows

---

    Code
      get_geoids(geography = "zcta", state = "WY", year = 2010, dataset = "decennial",
        geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2010 decennial Census
      Using Census Summary File 1
    Output
      Simple feature collection with 179 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.0569 ymin: 40.99461 xmax: -104.0522 ymax: 45.00589
      Geodetic CRS:  NAD83
      # A tibble: 179 x 4
         GEOID   NAME                 census_2010_pop
       * <chr>   <chr>                          <dbl>
       1 5682052 ZCTA5 82052, Wyoming              23
       2 5682731 ZCTA5 82731, Wyoming             135
       3 5682325 ZCTA5 82325, Wyoming             878
       4 5682225 ZCTA5 82225, Wyoming            2067
       5 5682430 ZCTA5 82430, Wyoming             100
       6 5683124 ZCTA5 83124, Wyoming             137
       7 5683114 ZCTA5 83114, Wyoming             828
       8 5682635 ZCTA5 82635, Wyoming             207
       9 5682450 ZCTA5 82450, Wyoming             177
      10 5682842 ZCTA5 82842, Wyoming             843
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-105.2875 41.06376, -105.2877 41.06378, -105.2881 41.06386, -105.2886 41.~
       2 (((-105.0767 44.99458, -105.0767 44.98802, -105.0767 44.97935, -105.0767 44.~
       3 (((-107.6938 41.22284, -107.694 41.22353, -107.6941 41.22411, -107.6943 41.2~
       4 (((-104.0531 42.97149, -104.0531 42.96754, -104.0531 42.9537, -104.0531 42.9~
       5 (((-108.1775 43.80303, -108.1778 43.80295, -108.1784 43.80269, -108.1786 43.~
       6 (((-110.4015 41.75898, -110.4012 41.75951, -110.401 41.75979, -110.4007 41.7~
       7 (((-111.0464 41.88595, -111.0465 41.88932, -111.0465 41.8904, -111.0465 41.8~
       8 (((-106.2149 43.42703, -106.2139 43.42737, -106.2138 43.42742, -106.2113 43.~
       9 (((-109.5092 44.47737, -109.5092 44.47743, -109.5092 44.4775, -109.5092 44.4~
      10 (((-106.9343 44.55846, -106.9472 44.55859, -106.9812 44.55893, -106.9817 44.~
      # i 169 more rows

---

    Code
      get_geoids(geography = "zcta", state = "WY", year = 2020, dataset = "decennial",
        geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2020 decennial Census
      Using the Demographic and Housing Characteristics File
    Output
      Simple feature collection with 180 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -111.2898 ymin: 40.51412 xmax: -103.5487 ymax: 45.43326
      Geodetic CRS:  NAD83
      # A tibble: 180 x 4
         GEOID NAME                        census_2020_pop
       * <chr> <chr>                                 <dbl>
       1 82630 ZCTA5 82630, Wyoming                     21
       2 82225 ZCTA5 82225, Wyoming                   2077
       3 82729 ZCTA5 82729, Wyoming                   2221
       4 82712 ZCTA5 82712, Wyoming                    332
       5 82922 ZCTA5 82922, Wyoming                    304
       6 82930 ZCTA5 82930 (part), Wyoming           13860
       7 82224 ZCTA5 82224, Wyoming                     30
       8 83414 ZCTA5 83414, Wyoming                    546
       9 82009 ZCTA5 82009, Wyoming                  35700
      10 82201 ZCTA5 82201, Wyoming                   6395
                                                                              geometry
       *                                                            <MULTIPOLYGON [°]>
       1 (((-107.4129 43.28586, -107.4116 43.28912, -107.4111 43.29254, -107.4112 43.~
       2 (((-104.6233 43.04996, -104.6178 43.05381, -104.615 43.05482, -104.6125 43.0~
       3 (((-104.7426 44.47215, -104.7412 44.47341, -104.7384 44.47328, -104.7341 44.~
       4 (((-104.2256 44.49522, -104.224 44.49973, -104.2228 44.50279, -104.2193 44.5~
       5 (((-110.589 43.04019, -110.5881 43.04029, -110.5879 43.12493, -110.588 43.12~
       6 (((-111.0466 41.17712, -111.0466 41.20092, -111.0466 41.24811, -111.0466 41.~
       7 (((-104.9786 42.91115, -104.9785 42.9193, -104.978 42.92013, -104.9723 42.92~
       8 (((-111.0491 44.13237, -111.0007 44.13238, -110.7706 44.13226, -110.7685 44.~
       9 (((-105.28 41.27057, -105.2794 41.27319, -105.2785 41.27391, -105.2759 41.27~
      10 (((-105.6913 41.70677, -105.6899 41.70852, -105.687 41.71097, -105.6833 41.7~
      # i 170 more rows

---

    Code
      get_geoids(geography = "state", year = 2005, dataset = "acs1", geometry = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2005 1-year ACS
      The 1-year ACS provides data for geographies with populations of 65,000 and greater.
    Output
      # A tibble: 52 x 3
         GEOID NAME                 acs1_2005_pop
         <chr> <chr>                        <dbl>
       1 01    Alabama                    4442558
       2 02    Alaska                      641724
       3 04    Arizona                    5829839
       4 05    Arkansas                   2701431
       5 06    California                35278768
       6 08    Colorado                   4562244
       7 09    Connecticut                3394751
       8 10    Delaware                    818587
       9 11    District of Columbia        515118
      10 12    Florida                   17382511
      # i 42 more rows

---

    Code
      get_geoids(geography = "county", state = c("DE", "DC"), year = 2012, dataset = "acs3",
      geometry = TRUE, progress_bar = FALSE)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2010-2012 3-year ACS
      The 3-year ACS provides data for geographies with populations of 20,000 and greater.
    Output
      Simple feature collection with 4 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -77.11976 ymin: 38.45113 xmax: -74.98416 ymax: 39.83952
      Geodetic CRS:  NAD83
        GEOID                                       NAME acs3_2012_pop
      1 10001                      Kent County, Delaware        165306
      2 10003                New Castle County, Delaware        542490
      3 10005                    Sussex County, Delaware        200555
      4 11001 District of Columbia, District of Columbia        618777
                              geometry
      1 MULTIPOLYGON (((-75.66547 3...
      2 MULTIPOLYGON (((-75.76596 3...
      3 MULTIPOLYGON (((-75.70157 3...
      4 MULTIPOLYGON (((-77.11975 3...

---

    Code
      get_geoids(geography = "tract", geoid = c("10", "39035"), year = 2012, dataset = "acs5",
      geometry = TRUE, progress_bar = FALSE)
    Message
      
      Preliminary tidycensus call beginning...
      Getting data from the 2008-2012 5-year ACS
      
      4 call(s) to tidycensus beginning.
      Getting data from the 2008-2012 5-year ACS
      Getting data from the 2008-2012 5-year ACS
      Getting data from the 2008-2012 5-year ACS
      Getting data from the 2008-2012 5-year ACS
    Output
      Simple feature collection with 665 features and 3 fields
      Geometry type: MULTIPOLYGON
      Dimension:     XY
      Bounding box:  xmin: -81.97126 ymin: 38.45113 xmax: -74.98416 ymax: 42.0964
      Geodetic CRS:  NAD83
      First 10 features:
                GEOID                                             NAME acs5_2012_pop
      448 10003013612 Census Tract 136.12, New Castle County, Delaware          6278
      449 10003012900    Census Tract 129, New Castle County, Delaware          5001
      450 10003010101 Census Tract 101.01, New Castle County, Delaware          4425
      451 10003010200    Census Tract 102, New Castle County, Delaware          1989
      452 10003010300    Census Tract 103, New Castle County, Delaware          2702
      453 10003014903 Census Tract 149.03, New Castle County, Delaware          8243
      454 10003015200    Census Tract 152, New Castle County, Delaware          6591
      455 10003015400    Census Tract 154, New Castle County, Delaware          3021
      456 10003010400    Census Tract 104, New Castle County, Delaware          4569
      457 10003014808 Census Tract 148.08, New Castle County, Delaware          6206
                                geometry
      448 MULTIPOLYGON (((-75.70662 3...
      449 MULTIPOLYGON (((-75.61177 3...
      450 MULTIPOLYGON (((-75.46947 3...
      451 MULTIPOLYGON (((-75.47606 3...
      452 MULTIPOLYGON (((-75.48065 3...
      453 MULTIPOLYGON (((-75.70607 3...
      454 MULTIPOLYGON (((-75.60868 3...
      455 MULTIPOLYGON (((-75.56416 3...
      456 MULTIPOLYGON (((-75.49045 3...
      457 MULTIPOLYGON (((-75.74609 3...

