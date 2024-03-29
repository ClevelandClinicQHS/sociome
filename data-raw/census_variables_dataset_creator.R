library(tidyverse)

decennial_vars <-
  tibble::tribble(
    ~variable, ~sumfile, ~year, ~description,
    
    # 2010 Decennial Census
    "P018001", "sf1",    2010L, "Total households",
    
    "H003002", "sf1",    2010L, "Occupied housing units",
    "H014002", "sf1",    2010L, "Owner-occupied housing units",
    
    "P020002", "sf1",    2010L, "Households w/ 1+ children",
    "P020008", "sf1",    2010L, "Households w/ 1+ children, single parent",
    
    
    # 2000 Decennial Census
    "P015001", "sf1",    2000L, "Total households",
    
    "H004001", "sf1",    2000L, "Occupied housing units",
    "H004002", "sf1",    2000L, "Owner-occupied housing units",
    
    "P077001", "sf3",    2000L, "Median family income",
    
    "H091001", "sf3",    2000L, "Median monthly costs - housing units w/ mortgage",
    
    "H063001", "sf3",    2000L, "Median gross rent (cash-paid renter-occupied)",
    
    "H085001", "sf3",    2000L, "Median value of owner-occupied housing units",
    
    "P090001", "sf3",    2000L, "Total families",
    "P090002", "sf3",    2000L, "Families w/ income below poverty level",
    
    "P052002", "sf3",    2000L, "Households w/ income under $10k",
    "P052011", "sf3",    2000L, "Households w/ income $50k-under $60k",
    "P052012", "sf3",    2000L, "Households w/ income $60k-under $75k",
    "P052013", "sf3",    2000L, "Households w/ income $75k-under $100k",
    "P052014", "sf3",    2000L, "Households w/ income $100k-under $125k",
    "P052015", "sf3",    2000L, "Households w/ income $125k-under $150k",
    "P052016", "sf3",    2000L, "Households w/ income $150k-under $200k",
    "P052017", "sf3",    2000L, "Households w/ income $200k or more",
    
    "P088001", "sf3",    2000L, "Population w/ known poverty status",
    "P088002", "sf3",    2000L, "People w/ income to poverty level ratio under .5",
    "P088003", "sf3",    2000L, "People w/ income to poverty level ratio .5-.74",
    "P088004", "sf3",    2000L, "People w/ income to poverty level ratio .75-.99",
    "P088005", "sf3",    2000L, "People w/ income to poverty level ratio 1-1.24",
    "P088006", "sf3",    2000L, "People w/ income to poverty level ratio 1.25-1.49",
    
    "P019002", "sf1",    2000L, "Households w/ 1+ children",
    "P019005", "sf1",    2000L, "Households w/ 1+ children, single parent",
    
    "H044001", "sf3",    2000L, "Occupied housing units (tenure/vehicles)",
    "H044003", "sf3",    2000L, "Owner-occupied housing units with no vehicle",
    "H044010", "sf3",    2000L, "Renter-occupied housing units with no vehicle",
    
    "P050001", "sf3",    2000L, "Employed civilians age 16+",
    "P050003", "sf3",    2000L, "Civilian males age 16+ in white-collar occupations",
    "P050050", "sf3",    2000L, "Civilian females age 16+ in white-collar occupations",
    
    "P043005", "sf3",    2000L, "Male civilian labor force age 16+",
    "P043012", "sf3",    2000L, "Female civilian labor force age 16+",
    "P043007", "sf3",    2000L, "Male unemployed civilian labor force age 16+",
    "P043014", "sf3",    2000L, "Female unemployed civilian labor force age 16+",
    
    "P037001", "sf3",    2000L, "Population age 25+",
    "P037003", "sf3",    2000L, "Males 25+ w/ no education",
    "P037020", "sf3",    2000L, "Females 25+ w/ no education",
    "P037004", "sf3",    2000L, "Males 25+ w/ nursery-4th grade as highest education",
    "P037021", "sf3",    2000L, "Females 25+ w/ nursery-4th grade as highest education",
    "P037005", "sf3",    2000L, "Males 25+ w/ 5th-6th grade as highest education",
    "P037022", "sf3",    2000L, "Females 25+ w/ 5th-6th grade as highest education",
    "P037006", "sf3",    2000L, "Males 25+ w/ 7th-8th as highest education",
    "P037023", "sf3",    2000L, "Females 25+ w/ 7th-8th as highest education",
    "P037011", "sf3",    2000L, "Males 25+ w/ diploma or alternate as highest education",
    "P037028", "sf3",    2000L, "Females 25+ w/ diploma or alternate as highest education",
    "P037012", "sf3",    2000L, "Males 25+ w/ <1 yr college as highest education",
    "P037029", "sf3",    2000L, "Females 25+ w/ <1 yr college as highest education",
    "P037013", "sf3",    2000L, "Males 25+ w/ 1+ yrs college w/o degree",
    "P037030", "sf3",    2000L, "Females 25+ w/ 1+ yrs college w/o degree",
    "P037014", "sf3",    2000L, "Males 25+ w/ Associate's as highest degree",
    "P037031", "sf3",    2000L, "Females 25+ w/ Associate's as highest degree",
    "P037015", "sf3",    2000L, "Males 25+ w/ Bachelor's as highest degree",
    "P037032", "sf3",    2000L, "Females 25+ w/ Bachelor's as highest degree",
    "P037016", "sf3",    2000L, "Males 25+ w/ Master's as highest degree",
    "P037033", "sf3",    2000L, "Females 25+ w/ Master's as highest degree",
    "P037017", "sf3",    2000L, "Males 25+ w/ Professional degree as highest degree",
    "P037034", "sf3",    2000L, "Females 25+ w/ Professional degree as highest degree",
    "P037018", "sf3",    2000L, "Males 25+ w/ Doctorate as highest degree",
    "P037035", "sf3",    2000L, "Females 25+ w/ Doctorate as highest degree",
    
    "H020001", "sf3",    2000L, "Occupied housing units (tenure/people per room)",
    "H020005", "sf3",    2000L, "Owner-occupied housing units with 1.01-1.5 per room",
    "H020006", "sf3",    2000L, "Owner-occupied housing units with 1.51-2 per room",
    "H020007", "sf3",    2000L, "Owner-occupied housing units with over 2 per room",
    "H020011", "sf3",    2000L, "Renter-occupied housing units with 1.01-1.5 per room",
    "H020012", "sf3",    2000L, "Renter-occupied housing units with 1.51-2 per room",
    "H020013", "sf3",    2000L, "Renter-occupied housing units with over 2 per room",
    
    
    # 1990 Decennial Census
    "P0030001", "sf1",   1990L, "Total households",
    
    "H0020001", "sf1",   1990L, "Occupied housing units",
    "H0030001", "sf1",   1990L, "Owner-occupied housing units",
    
    "P107A001", "sf3",   1990L, "Median family income",
    
    "H052A001",	"sf3",   1990L, "Median monthly costs - housing units w/ mortgage",
    
    "H043A001",	"sf3",   1990L, "Median gross rent (cash-paid renter-occupied)",
    
    "H023B001", "sf1",   1990L, "Median value of owner-occupied housing units",
    
    "P0040001",	"sf3",   1990L, "Total families",
    "P1230013",	"sf3",   1990L, "Fams w/ inc <pov lv, married couple all children <5 yrs",
    "P1230014",	"sf3",   1990L, "Fams w/ inc <pov lv, married couple all children 5-17 yrs",
    "P1230015",	"sf3",   1990L, "Fams w/ inc <pov lv, married couple children <5 & 5-17 yrs",
    "P1230016",	"sf3",   1990L, "Fams w/ inc <pov lv, married couple no children <18",
    "P1230017",	"sf3",   1990L, "Fams w/ inc <pov lv, male no wife all children <5 yrs",
    "P1230018",	"sf3",   1990L, "Fams w/ inc <pov lv, male no wife all children 5-17 yrs",
    "P1230019",	"sf3",   1990L, "Fams w/ inc <pov lv, male no wife children <5 & 5-17 yrs",
    "P1230020",	"sf3",   1990L, "Fams w/ inc <pov lv, male no wife no children <18",
    "P1230021",	"sf3",   1990L, "Fams w/ inc <pov lv, female no husband all children <5 yrs", 
    "P1230022",	"sf3",   1990L, "Fams w/ inc <pov lv, female no husband all children 5-17 yrs", 
    "P1230023",	"sf3",   1990L, "Fams w/ inc <pov lv, female no husband children <5 & 5-17 yrs", 
    "P1230024",	"sf3",   1990L, "Fams w/ inc <pov lv, female no husband no children <18",
    
    "P0800001",	"sf3",   1990L, "Households w/ income under $5k",
    "P0800002",	"sf3",   1990L, "Households w/ income $5k-under $10k",
    "P0800019",	"sf3",   1990L, "Households w/ income $50k-under $55k",
    "P0800020",	"sf3",   1990L, "Households w/ income $55k-under $60k",
    "P0800021",	"sf3",   1990L, "Households w/ income $60k-under $75k",
    "P0800022",	"sf3",   1990L, "Households w/ income $75k-under $100k",
    "P0800023",	"sf3",   1990L, "Households w/ income $100k-under $125k",
    "P0800024",	"sf3",   1990L, "Households w/ income $125k-under $150k",
    "P0800025",	"sf3",   1990L, "Households w/ income $150k or more",
    
    "P1210001", "sf3",   1990L, "People w/ income to poverty level ratio under .5",
    "P1210002", "sf3",   1990L, "People w/ income to poverty level ratio .5-.74",
    "P1210003", "sf3",   1990L, "People w/ income to poverty level ratio .75-.99",
    "P1210004", "sf3",   1990L, "People w/ income to poverty level ratio 1-1.24",
    "P1210005", "sf3",   1990L, "People w/ income to poverty level ratio 1.25-1.49",
    "P1210006", "sf3",   1990L, "People w/ income to poverty level ratio 1.5-1.74",
    "P1210007", "sf3",   1990L, "People w/ income to poverty level ratio 1.75-1.84",
    "P1210008", "sf3",   1990L, "People w/ income to poverty level ratio 1.85-1.99",
    "P1210009", "sf3",   1990L, "People w/ income to poverty level ratio 2+",
    
    "P0180001", "sf1",   1990L, "Family households w/ 1+ children, Married couple",
    "P0180002", "sf1",   1990L, "Family households w/ 1+ children, single dad",
    "P0180003", "sf1",   1990L, "Family households w/ 1+ children, single mom",
    "P0180004", "sf1",   1990L, "Nonfamily households w/ 1+ children, male householder",
    "P0180005", "sf1",   1990L, "Nonfamily households w/ 1+ children, female householder",
    
    "H0410001", "sf3",   1990L, "Occupied housing units w/ householder 15-64 yrs w/ no vehicle",
    "H0410002", "sf3",   1990L, "Occupied housing units w/ householder 15-64 yrs w/ 1+ vehicle",
    "H0410003", "sf3",   1990L, "Occupied housing units w/ householder 65+ yrs w/ no vehicle",
    "H0410004", "sf3",   1990L, "Occupied housing units w/ householder 65+ yrs w/ 1+ vehicle",
    
    "P0780001", "sf3",   1990L, "People 16+ in executive, admin, management occupations",
    "P0780002", "sf3",   1990L, "People 16+ in professional specialty occupations",
    "P0700002", "sf3",   1990L, "Employed civilian males age 16+",
    "P0700006", "sf3",   1990L, "Employed civilian females age 16+",
    
    "P0700003", "sf3",   1990L, "Unemployed civilian males age 16+",
    "P0700007", "sf3",   1990L, "Unemployed civilian females age 16+",
    
    "P0570001", "sf3",   1990L, "People 25+ attained <9th grade education",
    "P0570002", "sf3",   1990L, "People 25+ attained 9-12 grade, no degree",
    "P0570003", "sf3",   1990L, "People 25+ w/ high school diploma or equivalent",
    "P0570004", "sf3",   1990L, "People 25+ w/ some college, no degree",
    "P0570005", "sf3",   1990L, "People 25+ w/ associate degree",
    "P0570006", "sf3",   1990L, "People 25+ w/ bachelor's degree",
    "P0570007", "sf3",   1990L, "People 25+ w/ graduate or professional degree",
    
    "H0210001", "sf1",   1990L, "Occupied housing units w/ .5 or less people/room",
    "H0210002", "sf1",   1990L, "Occupied housing units w/ over .5 to 1 people/room",
    "H0210003", "sf1",   1990L, "Occupied housing units w/ over 1 to 1.5 people/room",
    "H0210004", "sf1",   1990L, "Occupied housing units w/ over 1.5 to 2 people/room",
    "H0210005", "sf1",   1990L, "Occupied housing units w/ 2 or more people/room"
  )

acs_vars <-
  tibble::tribble(
    ~variable,    ~description,                                               ~set1, ~set2, ~set3, ~set4, ~set5, ~set6, ~set7, ~dec2010,
    "B11005_001",	"Total households",                                 	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  FALSE,
    "B15002_001",	"Population age 25+",	                                      FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_003",	"Males 25+ w/ no education",	                              FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_004",	"Males 25+ w/ nursery-4th grade as highest education",	    FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_005",	"Males 25+ w/ 5th-6th grade as highest education",	        FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_006",	"Males 25+ w/ 7th-8th as highest education",	              FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_011",	"Males 25+ w/ diploma or alternate as highest education",	  FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_012",	"Males 25+ w/ <1 yr college as highest education",	        FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_013",	"Males 25+ w/ 1+ yrs college w/o degree",	                  FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_014",	"Males 25+ w/ Associate's as highest degree",	              FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_015",	"Males 25+ w/ Bachelor's as highest degree",	              FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_016",	"Males 25+ w/ Master's as highest degree",	                FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_017",	"Males 25+ w/ Professional degree as highest degree",	      FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_018",	"Males 25+ w/ Doctorate as highest degree",	                FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_020",	"Females 25+ w/ no education",	                            FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_021",	"Females 25+ w/ nursery-4th grade as highest education",	  FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_022",	"Females 25+ w/ 5th-6th grade as highest education",	      FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_023",	"Females 25+ w/ 7th-8th as highest education",	            FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_028",	"Females 25+ w/ diploma or alternate as highest education", FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_029",	"Females 25+ w/ <1 yr college as highest education",	      FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_030",	"Females 25+ w/ 1+ yrs college w/o degree",	                FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_031",	"Females 25+ w/ Associate's as highest degree",	            FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_032",	"Females 25+ w/ Bachelor's as highest degree",	            FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_033",	"Females 25+ w/ Master's as highest degree",	              FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_034",	"Females 25+ w/ Professional degree as highest degree",	    FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15002_035",	"Females 25+ w/ Doctorate as highest degree",	              FALSE, FALSE, TRUE,	 FALSE,	TRUE,  FALSE, TRUE,  TRUE,
    "B15003_001",	"Population age 25+",	                                      TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_002",	"People 25+ w/ no education",	                              TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_003",	"People 25+ w/ nursery school as highest education",	      TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_004",	"People 25+ w/ kindergarten as highest education",	        TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_005",	"People 25+ w/ 1st grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_006",	"People 25+ w/ 2nd grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_007",	"People 25+ w/ 3rd grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_008",	"People 25+ w/ 4th grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_009",	"People 25+ w/ 5th grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_010",	"People 25+ w/ 6th grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_011",	"People 25+ w/ 7th grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_012",	"People 25+ w/ 8th grade as highest education",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_017",	"People 25+ w/ diploma as highest education",	              TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_018",	"People 25+ w/ GED or alternate as highest education",	    TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_019",	"People 25+ w/ <1 yr college as highest education",	        TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_020",	"People 25+ w/ 1+ yrs college w/o degree",	                TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_021",	"People 25+ w/ Associate's as highest degree",	            TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_022",	"People 25+ w/ Bachelor's as highest degree",	              TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_023",	"People 25+ w/ Master's as highest degree",	                TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_024",	"People 25+ w/ Professional degree as highest degree",	    TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B15003_025",	"People 25+ w/ Doctorate as highest degree",	              TRUE,  TRUE,	FALSE, TRUE,	FALSE, TRUE,  FALSE, FALSE,
    "B17010_001",	"Total families",	                                          TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B17010_002",	"Families w/ income below poverty level",	                  TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_002",	"Households w/ income under $10k",	                        TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_011",	"Households w/ income $50k-under $60k",	                    TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_012",	"Households w/ income $60k-under $75k",	                    TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_013",	"Households w/ income $75k-under $100k",	                  TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_014",	"Households w/ income $100k-under $125k",	                  TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_015",	"Households w/ income $125k-under $150k",	                  TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_016",	"Households w/ income $150k-under $200k",	                  TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19001_017",	"Households w/ income $200k or more",	                      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B19013_001", "Median household income",                                  FALSE, TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "B19113_001",	"Median family income",   	                                TRUE,  FALSE, TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_006",	"Male civilian labor force age 16-19 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_008",	"Unemployed Male civilian labor force age 16-19 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_013",	"Male civilian labor force age 20-21 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_015",	"Unemployed Male civilian labor force age 20-21 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_020",	"Male civilian labor force age 22-24 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_022",	"Unemployed Male civilian labor force age 22-24 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_027",	"Male civilian labor force age 25-29 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_029",	"Unemployed Male civilian labor force age 25-29 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_034",	"Male civilian labor force age 30-34 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_036",	"Unemployed Male civilian labor force age 30-34 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_041",	"Male civilian labor force age 35-44 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_043",	"Unemployed Male civilian labor force age 35-44 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_048",	"Male civilian labor force age 45-54 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_050",	"Unemployed Male civilian labor force age 45-54 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_055",	"Male civilian labor force age 55-59 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_057",	"Unemployed Male civilian labor force age 55-59 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_062",	"Male civilian labor force age 60-61 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_064",	"Unemployed Male civilian labor force age 60-61 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_069",	"Male civilian labor force age 62-64 yrs",	                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_071",	"Unemployed Male civilian labor force age 62-64 yrs",	      FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_074",	"Male labor force age 65-69",	                              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_076",	"Unemployed Male labor force age 65-69 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_079",	"Male labor force age 70-74",	                              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_081",	"Unemployed Male labor force age 70-74 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_084",	"Male labor force age 75+",	                                FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_086",	"Unemployed Male labor force age 75+",	                    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_092",	"Female civilian labor force age 16-19 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_094",	"Unemployed Female civilian labor force age 16-19 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_099",	"Female civilian labor force age 20-21 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_101",	"Unemployed Female civilian labor force age 20-21 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_106",	"Female civilian labor force age 22-24 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_108",	"Unemployed Female civilian labor force age 22-24 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_113",	"Female civilian labor force age 25-29 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_115",	"Unemployed Female civilian labor force age 25-29 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_120",	"Female civilian labor force age 30-34 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_122",	"Unemployed Female civilian labor force age 30-34 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_127",	"Female civilian labor force age 35-44 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_129",	"Unemployed Female civilian labor force age 35-44 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_134",	"Female civilian labor force age 45-54 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_136",	"Unemployed Female civilian labor force age 45-54 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_141",	"Female civilian labor force age 55-59 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_143",	"Unemployed Female civilian labor force age 55-59 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_148",	"Female civilian labor force age 60-61 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_150",	"Unemployed Female civilian labor force age 60-61 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_155",	"Female civilian labor force age 62-64 yrs",	              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_157",	"Unemployed Female civilian labor force age 62-64 yrs",	    FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_160",	"Female labor force age 65-69",	                            FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_162",	"Unemployed Female labor force age 65-69 yrs",	            FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_165",	"Female labor force age 70-74",	                            FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_167",	"Unemployed Female labor force age 70-74 yrs",	            FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_170",	"Female labor force age 75+",	                              FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B23001_172",	"Unemployed Female labor force age 75+",	                  FALSE, FALSE, FALSE, TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B11005_002",	"Households w/ 1+ person <18",                      	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  FALSE,
    "B11005_005",	"Family households w/ single parent & 1+ person <18",       TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  FALSE,
    "B23025_003",	"Civilian labor force age 16+",	                            TRUE,  TRUE,	TRUE,	 FALSE,	FALSE, FALSE, FALSE, FALSE,
    "B23025_005",	"Unemployed civilian labor force age 16+",	                TRUE,  TRUE,	TRUE,	 FALSE,	FALSE, FALSE, FALSE, FALSE,
    "B25003_001",	"Occupied housing units",	                                  TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  FALSE,
    "B25003_002",	"Owner-occupied housing units",	                            TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  FALSE,
    "B25014_001",	"Occupied housing units (tenure/people per room)",	        TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25014_005",	"Owner-occupied housing units with 1.01-1.5 per room",	    TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25014_006",	"Owner-occupied housing units with 1.51-2 per room",	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25014_007",	"Owner-occupied housing units with over 2 per room",	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25014_011",	"Renter-occupied housing units with 1.01-1.5 per room",	    TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25014_012",	"Renter-occupied housing units with 1.51-2 per room",	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25014_013",	"Renter-occupied housing units with over 2 per room",	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25044_001",	"Occupied housing units (tenure/vehicles)",	                TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25044_003",	"Owner-occupied housing units with no vehicle",	            TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25044_010",	"Renter-occupied housing units with no vehicle",	          TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25064_001",	"Median gross rent (cash-paid renter-occupied)",	          TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25077_001",	"Median value of owner-occupied housing units",	            TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "B25088_002",	"Median monthly costs - housing units w/ mortgage",	        TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C17002_001",	"Population w/ known poverty status",	                      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C17002_002",	"People w/ income to poverty level ratio under .5",	        TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C17002_003",	"People w/ income to poverty level ratio .5-.99",	          TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C17002_004",	"People w/ income to poverty level ratio 1-1.24",	          TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C17002_005",	"People w/ income to poverty level ratio 1.25-1.49",	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C24010_001",	"Employed civilians age 16+",	                              TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C24010_003",	"Civilian males age 16+ in white-collar occupations",	      TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  TRUE,  TRUE,  TRUE,
    "C24010_039",	"Civilian females age 16+ in white-collar occupations",	    TRUE,  TRUE,	TRUE,	 TRUE,	TRUE,  FALSE, FALSE, TRUE,
    "C24010_040",	"Civilian females age 16+ in white-collar occupations",	    FALSE, FALSE,	FALSE, FALSE,	FALSE, TRUE,  TRUE,  FALSE
  )


acs_age_sex_race_ethnicity_vars <-
  bind_rows(
    tidycensus::load_variables(year = 2020, dataset = "acs5", cache = TRUE) %>% 
      filter(str_detect(name, "^B01001_")) %>% 
      transmute(
        variable = name,
        description = str_replace(label, "^Estimate!!Total:!!", ""),
        description = str_replace(description, "^(\\w+ale):", "\\1s"),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = c("total", description[-1]),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2020, dataset = "acs5", cache = TRUE) %>% 
      filter(
        str_detect(name, "^B03002_"),
        str_detect(label, ":!!Two or more races:!!", negate = TRUE)
      ) %>%
      slice(-1) %>% 
      transmute(
        variable = name,
        description = str_replace(label, "^Estimate!!Total:!!", ""),
        description = str_replace_all(description, ":", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      )
  )

decennial_age_sex_race_ethnicity_vars <-
  bind_rows(
    tidycensus::load_variables(year = 2000, dataset = "sf1", cache = TRUE) %>% 
      filter(str_detect(name, "^P012\\d{3}$")) %>% 
      transmute(
        year = 2000L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, "^(\\w+ale)", "\\1s"),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2000, dataset = "sf1", cache = TRUE) %>% 
      filter(str_detect(name, "^P008\\d{3}$")) %>% 
      slice(-1) %>% 
      transmute(
        year = 2000L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2010, dataset = "sf1", cache = TRUE) %>% 
      filter(str_detect(name, "^P012\\d{3}$")) %>% 
      transmute(
        year = 2010L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, "^(\\w+ale)", "\\1s"),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      ),
    tidycensus::load_variables(year = 2010, dataset = "sf1", cache = TRUE) %>% 
      filter(str_detect(name, "^P005\\d{3}$")) %>% 
      slice(-1) %>% 
      transmute(
        year = 2010L,
        variable = name,
        description = str_replace(label, "^Total!!", ""),
        description = str_replace(description, " years", ""),
        description = str_replace_all(description, "!!| ", "_"),
        description = tolower(description)
      )
  )

usethis::use_data(acs_vars,
                  decennial_vars,
                  acs_age_sex_race_ethnicity_vars,
                  decennial_age_sex_race_ethnicity_vars,
                  overwrite = TRUE, compress = "xz")
