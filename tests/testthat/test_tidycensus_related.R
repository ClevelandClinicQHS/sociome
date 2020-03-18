context("Test tidycensus-related functions")

test_that("get_adi() works", {
  
  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  
  expect_equivalent(
    get_adi(
      geography = "state",
      year = 2017,
      dataset = "acs5",
      cache_tables = FALSE
    ) %>% as.data.frame(),
    dplyr::tibble(
      GEOID =
        c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
          "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
          "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
          "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
          "49", "50", "51", "53", "54", "55", "56", "72"),
      NAME = 
        c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
          "Connecticut", "Delaware", "District of Columbia", "Florida",
          "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
          "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
          "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
          "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
          "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
          "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
          "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
          "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico"),
      ADI =
        c(118.805816750111, 78.9843298877816, 108.276755959634,
          119.077262313526, 92.4403712600195, 83.2468859276432,
          81.200964777788, 93.0843827967164, 79.5027881192426, 
          107.074064967396, 110.025785909392, 68.3231266156302,
          101.879364786388, 98.3049144828643, 107.100124682063,
          95.272646415142, 96.3048127292274, 118.836460604923,
          120.874576505555, 97.6078078512824, 77.0408355062725, 
          77.9899230305367, 105.689474166643, 83.6965602995459,
          130.843634925426, 104.353364442856, 95.8693770397552,
          94.3536391702125, 106.896876514336, 75.1784792511584,
          78.131182306305, 121.879490044139, 93.8904315459346, 
          109.705138697794, 87.696879521769, 105.276806177702,
          110.117759662194, 95.3158520286823, 97.7785508790391,
          97.3490666129875, 114.28876215544, 97.8887176662155,
          112.809917268262, 109.185868463237, 83.4335983455295,
          87.3552244697823, 84.6299015744505, 84.4966409120095, 
          120.162852552883, 93.4464087295901, 88.8815937832734,
          198.143948913714)
    ) %>% as.data.frame()
  )
})

test_that("calculate_adi() works correctly", {
  
  nm_adi_2017 <- calculate_adi(nmcounties2017)
  expect_identical(dim(nm_adi_2017), c(33L, 3L))
  expect_identical(dim(attr(nm_adi_2017, "loadings")), c(15L, 2L))
  
  nm_adi_2010 <- calculate_adi(nmcounties2010)
  expect_identical(dim(nm_adi_2010), c(33L, 3L))
  expect_identical(dim(attr(nm_adi_2010, "loadings")), c(15L, 2L))
  
  nm_adi_2000 <- calculate_adi(nmcounties2000)
  expect_identical(dim(nm_adi_2000), c(33L, 3L))
  expect_identical(dim(attr(nm_adi_2000, "loadings")), c(15L, 2L))
  
  nm_adi_1990 <- calculate_adi(nmcounties1990)
  expect_identical(dim(nm_adi_1990), c(33L, 3L))
  expect_identical(dim(attr(nm_adi_1990, "loadings")), c(15L, 2L))
  
})

test_that("calculate_adi() works correctly with data", {
  
  nm_adi_2017 <- calculate_adi(nmcounties2017, keep_indicators = TRUE)
  expect_identical(dim(nm_adi_2017), c(33L, 78L))
  expect_identical(dim(attr(nm_adi_2017, "loadings")), c(15L, 2L))
  
  nm_adi_2010 <- calculate_adi(nmcounties2010, keep_indicators = TRUE)
  expect_identical(dim(nm_adi_2010), c(33L, 132L))
  expect_identical(dim(attr(nm_adi_2010, "loadings")), c(15L, 2L))
  
  nm_adi_2000 <- calculate_adi(nmcounties2000, keep_indicators = TRUE)
  expect_identical(dim(nm_adi_2000), c(33L, 85L))
  expect_identical(dim(attr(nm_adi_2000, "loadings")), c(15L, 2L))
  
  nm_adi_1990 <- calculate_adi(nmcounties1990, keep_indicators = TRUE)
  expect_identical(dim(nm_adi_1990), c(33L, 83L))
  expect_identical(dim(attr(nm_adi_1990, "loadings")), c(15L, 2L))
  
})
