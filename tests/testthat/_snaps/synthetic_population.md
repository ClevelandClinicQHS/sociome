# synthetic_population() works

    Code
      synthetic_population(geography = "state", state = "UT", year = 2019, max_age = 87,
        rate = 10)
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2015-2019 5-year ACS
    Output
      # A tibble: 3,096,848 x 8
         GEOID NAME  age_sex             sex    age_lo age_hi    age
         <chr> <chr> <chr>               <chr>   <dbl>  <dbl>  <dbl>
       1 49    Utah  males_30_to_34      male       30     34 31.8  
       2 49    Utah  males_under_5       male        0      4  0.298
       3 49    Utah  males_25_to_29      male       25     29 29.8  
       4 49    Utah  females_85_and_over female     85     87 85.1  
       5 49    Utah  females_10_to_14    female     10     14 11.1  
       6 49    Utah  females_under_5     female      0      4  2.77 
       7 49    Utah  females_67_to_69    female     67     69 68.6  
       8 49    Utah  males_35_to_39      male       35     39 35.4  
       9 49    Utah  females_60_and_61   female     60     61 61.2  
      10 49    Utah  females_62_to_64    female     62     64 62.6  
         race_ethnicity                    
         <chr>                             
       1 not_hispanic_or_latino_asian_alone
       2 not_hispanic_or_latino_white_alone
       3 not_hispanic_or_latino_white_alone
       4 not_hispanic_or_latino_white_alone
       5 not_hispanic_or_latino_white_alone
       6 not_hispanic_or_latino_white_alone
       7 not_hispanic_or_latino_white_alone
       8 not_hispanic_or_latino_white_alone
       9 not_hispanic_or_latino_white_alone
      10 hispanic_or_latino                
      # i 3,096,838 more rows

---

    Code
      synthetic_population(geography = "block", geoid = c("51001", "40117957200",
        "490572105041"), year = 2000, dataset = "decennial")
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
      # A tibble: 44,416 x 8
         GEOID          
         <chr>          
       1 510019901001000
       2 510019901001000
       3 510019901001000
       4 510019901001000
       5 510019901001000
       6 510019901001000
       7 510019901001000
       8 510019901001000
       9 510019901001000
      10 510019901001000
         NAME                                                                   
         <chr>                                                                  
       1 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       2 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       3 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       4 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       5 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       6 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       7 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       8 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
       9 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
      10 Block 1000, Block Group 1, Census Tract 9901, Accomack County, Virginia
         age_sex          sex    age_lo age_hi   age
         <chr>            <chr>   <dbl>  <dbl> <dbl>
       1 females_75_to_79 female     75     79  77.9
       2 females_22_to_24 female     22     24  24.7
       3 males_70_to_74   male       70     74  71.4
       4 females_35_to_39 female     35     39  36.7
       5 females_55_to_59 female     55     59  59.6
       6 males_75_to_79   male       75     79  77.2
       7 females_62_to_64 female     62     64  63.2
       8 females_62_to_64 female     62     64  62.9
       9 females_30_to_34 female     30     34  33.4
      10 males_62_to_64   male       62     64  63.0
         race_ethnicity                                        
         <chr>                                                 
       1 not_hispanic_or_latino_white_alone                    
       2 not_hispanic_or_latino_white_alone                    
       3 not_hispanic_or_latino_white_alone                    
       4 not_hispanic_or_latino_black_or_african_american_alone
       5 not_hispanic_or_latino_white_alone                    
       6 not_hispanic_or_latino_white_alone                    
       7 not_hispanic_or_latino_white_alone                    
       8 not_hispanic_or_latino_white_alone                    
       9 not_hispanic_or_latino_white_alone                    
      10 not_hispanic_or_latino_black_or_african_american_alone
      # i 44,406 more rows

---

    Code
      synthetic_population(geography = "zcta", state = c("CA", "OH", "AL"), zcta = c(
        "90210", "44133", "44147", "44136", "01001", "01010"), year = 2013, dataset = "acs5")
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2009-2013 5-year ACS
    Condition
      Warning in `filter_ref_area()`:
      
      The following ZCTAs had no match in census data:
      01001,
      01010
    Output
      # A tibble: 96,989 x 8
         GEOID NAME        age_sex           sex    age_lo age_hi   age
         <chr> <chr>       <chr>             <chr>   <dbl>  <dbl> <dbl>
       1 90210 ZCTA5 90210 females_35_to_39  female     35     39  38.4
       2 90210 ZCTA5 90210 males_62_to_64    male       62     64  63.1
       3 90210 ZCTA5 90210 females_80_to_84  female     80     84  80.7
       4 90210 ZCTA5 90210 males_55_to_59    male       55     59  55.7
       5 90210 ZCTA5 90210 females_80_to_84  female     80     84  82.2
       6 90210 ZCTA5 90210 males_85_and_over male       85    115  89.1
       7 90210 ZCTA5 90210 males_55_to_59    male       55     59  58.0
       8 90210 ZCTA5 90210 females_65_and_66 female     65     66  66.7
       9 90210 ZCTA5 90210 males_55_to_59    male       55     59  59.2
      10 90210 ZCTA5 90210 males_67_to_69    male       67     69  70.0
         race_ethnicity                    
         <chr>                             
       1 not_hispanic_or_latino_white_alone
       2 not_hispanic_or_latino_white_alone
       3 not_hispanic_or_latino_white_alone
       4 not_hispanic_or_latino_asian_alone
       5 not_hispanic_or_latino_white_alone
       6 not_hispanic_or_latino_white_alone
       7 not_hispanic_or_latino_white_alone
       8 not_hispanic_or_latino_white_alone
       9 not_hispanic_or_latino_white_alone
      10 not_hispanic_or_latino_white_alone
      # i 96,979 more rows

---

    Code
      synthetic_population(geography = "county", state = "NY", county = c("New York",
        "Queen", "King", "Bronx", "Richmond"), year = 2005, dataset = "acs1")
    Message
      
      1 call(s) to tidycensus beginning.
      Getting data from the 2005 1-year ACS
      The 1-year ACS provides data for geographies with populations of 65,000 and greater.
    Output
      # A tibble: 7,956,113 x 8
         GEOID NAME                   age_sex           sex    age_lo age_hi   age
         <chr> <chr>                  <chr>             <chr>   <dbl>  <dbl> <dbl>
       1 36005 Bronx County, New York females_80_to_84  female     80     84 82.4 
       2 36005 Bronx County, New York females_18_and_19 female     18     19 18.9 
       3 36005 Bronx County, New York males_10_to_14    male       10     14 10.6 
       4 36005 Bronx County, New York males_35_to_39    male       35     39 36.5 
       5 36005 Bronx County, New York males_under_5     male        0      4  1.60
       6 36005 Bronx County, New York females_55_to_59  female     55     59 57.1 
       7 36005 Bronx County, New York females_50_to_54  female     50     54 53.7 
       8 36005 Bronx County, New York males_35_to_39    male       35     39 39.7 
       9 36005 Bronx County, New York females_5_to_9    female      5      9  8.23
      10 36005 Bronx County, New York males_5_to_9      male        5      9  9.76
         race_ethnicity                                                
         <chr>                                                         
       1 not_hispanic_or_latino_white_alone                            
       2 hispanic_or_latino                                            
       3 hispanic_or_latino                                            
       4 hispanic_or_latino                                            
       5 hispanic_or_latino                                            
       6 hispanic_or_latino                                            
       7 hispanic_or_latino                                            
       8 not_hispanic_or_latino_white_alone                            
       9 hispanic_or_latino                                            
      10 not_hispanic_or_latino_american_indian_and_alaska_native_alone
      # i 7,956,103 more rows

