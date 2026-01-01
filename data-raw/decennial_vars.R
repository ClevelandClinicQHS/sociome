## code to prepare `decennial_vars` dataset goes here

library(here)
library(dplyr)

decennial_vars <-
  tribble(
    ~variable, ~sumfile, ~year, ~description,

    # 2020 Decennial Census
    "P16_001N", "dhc",   2020L, "Total households",
    "H3_002N",  "dhc",   2020L, "Occupied housing units",
    "H10_002N", "dhc",   2020L, "Owner-occupied housing units",
    "P20_003N", "dhc",   2020L, "Households w/ 1+ children, married couple",
    "P20_006N", "dhc",   2020L, "Households w/ 1+ children, married couple",
    "P20_011N", "dhc",   2020L, "Households w/ 1+ children, single father",
    "P20_017N", "dhc",   2020L, "Households w/ 1+ children, single mother",

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

usethis::use_data(decennial_vars, overwrite = TRUE)
tools::resaveRdaFiles(
  here("data", "decennial_vars.rda"),
  compress = "auto"
)
