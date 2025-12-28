# synthetic_population() works

    Code
      synthetic_population(geography = "state", state = "UT", year = 2019)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2015-2019 5-year ACS
    Output
      # A tibble: 3,096,848 x 8
         GEOID NAME  age_sex          sex    age_lo age_hi   age
         <chr> <chr> <chr>            <chr>   <dbl>  <dbl> <dbl>
       1 49    Utah  males_67_to_69   male       67     69  69.8
       2 49    Utah  females_10_to_14 female     10     14  12.5
       3 49    Utah  males_10_to_14   male       10     14  10.8
       4 49    Utah  females_10_to_14 female     10     14  12.7
       5 49    Utah  females_35_to_39 female     35     39  35.1
       6 49    Utah  females_21       female     21     21  21.2
       7 49    Utah  females_22_to_24 female     22     24  22.2
       8 49    Utah  females_15_to_17 female     15     17  17.3
       9 49    Utah  females_67_to_69 female     67     69  67.0
      10 49    Utah  males_40_to_44   male       40     44  44.9
         race_ethnicity                                                   
         <chr>                                                            
       1 not_hispanic_or_latino_white_alone                               
       2 not_hispanic_or_latino_white_alone                               
       3 not_hispanic_or_latino_white_alone                               
       4 not_hispanic_or_latino_white_alone                               
       5 not_hispanic_or_latino_some_other_race_alone_or_two_or_more_races
       6 not_hispanic_or_latino_white_alone                               
       7 not_hispanic_or_latino_white_alone                               
       8 not_hispanic_or_latino_white_alone                               
       9 not_hispanic_or_latino_white_alone                               
      10 hispanic_or_latino                                               
      # i 3,096,838 more rows

---

    Code
      synthetic_population(geography = "state", state = "UT", year = 2019, max_age = 87,
        rate = 10)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2015-2019 5-year ACS
    Output
      # A tibble: 3,096,848 x 8
         GEOID NAME  age_sex          sex    age_lo age_hi   age
         <chr> <chr> <chr>            <chr>   <dbl>  <dbl> <dbl>
       1 49    Utah  females_25_to_29 female     25     29 29.9 
       2 49    Utah  females_75_to_79 female     75     79 78.8 
       3 49    Utah  males_35_to_39   male       35     39 35.9 
       4 49    Utah  females_10_to_14 female     10     14 12.4 
       5 49    Utah  females_35_to_39 female     35     39 37.1 
       6 49    Utah  males_10_to_14   male       10     14 10.7 
       7 49    Utah  females_5_to_9   female      5      9  8.12
       8 49    Utah  females_5_to_9   female      5      9  9.57
       9 49    Utah  females_62_to_64 female     62     64 64.8 
      10 49    Utah  males_40_to_44   male       40     44 42.1 
         race_ethnicity                    
         <chr>                             
       1 not_hispanic_or_latino_white_alone
       2 not_hispanic_or_latino_white_alone
       3 not_hispanic_or_latino_white_alone
       4 not_hispanic_or_latino_white_alone
       5 not_hispanic_or_latino_white_alone
       6 hispanic_or_latino                
       7 not_hispanic_or_latino_white_alone
       8 not_hispanic_or_latino_white_alone
       9 not_hispanic_or_latino_white_alone
      10 not_hispanic_or_latino_white_alone
      # i 3,096,838 more rows

---

    Code
      synthetic_population(geography = "tract", geoid = c("10", "51001", "51131",
        "24015", "24029", "24035", "24011", "24041", "24019", "24045", "24039",
        "24047"), year = 2000, dataset = "decennial")
    Message
      
      3 call(s) to tidycensus beginning.
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Using Census Summary File 1
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Using Census Summary File 1
      Getting data from the 2000 decennial Census
      Using Census Summary File 1
      Using Census Summary File 1
    Output
      # A tibble: 1,230,901 x 8
         GEOID       NAME                                    age_sex          sex   
         <chr>       <chr>                                   <chr>            <chr> 
       1 10001040100 Census Tract 401, Kent County, Delaware females_50_to_54 female
       2 10001040100 Census Tract 401, Kent County, Delaware females_40_to_44 female
       3 10001040100 Census Tract 401, Kent County, Delaware males_10_to_14   male  
       4 10001040100 Census Tract 401, Kent County, Delaware males_40_to_44   male  
       5 10001040100 Census Tract 401, Kent County, Delaware males_45_to_49   male  
       6 10001040100 Census Tract 401, Kent County, Delaware females_20       female
       7 10001040100 Census Tract 401, Kent County, Delaware males_5_to_9     male  
       8 10001040100 Census Tract 401, Kent County, Delaware females_35_to_39 female
       9 10001040100 Census Tract 401, Kent County, Delaware males_30_to_34   male  
      10 10001040100 Census Tract 401, Kent County, Delaware males_5_to_9     male  
         age_lo age_hi   age
          <dbl>  <dbl> <dbl>
       1     50     54 52.1 
       2     40     44 40.9 
       3     10     14 11.3 
       4     40     44 44.7 
       5     45     49 46.3 
       6     20     20 20.2 
       7      5      9 10.00
       8     35     39 38.6 
       9     30     34 34.6 
      10      5      9  5.28
         race_ethnicity                                                   
         <chr>                                                            
       1 not_hispanic_or_latino_white_alone                               
       2 not_hispanic_or_latino_white_alone                               
       3 not_hispanic_or_latino_white_alone                               
       4 not_hispanic_or_latino_white_alone                               
       5 not_hispanic_or_latino_white_alone                               
       6 not_hispanic_or_latino_some_other_race_alone_or_two_or_more_races
       7 not_hispanic_or_latino_white_alone                               
       8 not_hispanic_or_latino_white_alone                               
       9 not_hispanic_or_latino_white_alone                               
      10 hispanic_or_latino                                               
      # i 1,230,891 more rows

