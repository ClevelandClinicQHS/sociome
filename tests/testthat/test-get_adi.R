test_that("get_adi works", {

  skip_if(Sys.getenv("CENSUS_API_KEY") == "")
  set.seed(20251229)

  # set1
  expect_snapshot(
    get_adi(
      geography = "tract",
      state = "oh",
      county = "cuyahoga",
      year = 2010,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # set2
  expect_warning(
    set2_raw_only <-
      get_adi(
        geography = "block group",
        state = "DE",
        year = 2015,
        dataset = "acs5",
        keep_indicators = TRUE,
        cache_tables = FALSE,
        raw_data_only = TRUE
      ),
    "(?i)median (family|household) income"
  )
  expect_warning(
    set2_adi_results <- calculate_adi(set2_raw_only),
    "(?i)median (family|household) income"
  )
  expect_snapshot(set2_adi_results)

  # set3
  expect_snapshot(
    get_adi(
      geography = "zcta",
      zcta = c("99", "44147"),
      state =  c("AK", "OH"),
      year = 2011,
      dataset = "acs5",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # set4
  expect_snapshot(
    get_adi(
      geography = "state",
      year = 2010,
      dataset = "acs1",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # set5
  expect_snapshot(
    get_adi(
      geography = "county",
      geoid = c("39035136101", "35"),
      year = 2010,
      dataset = "acs5",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # set6
  expect_snapshot(
    get_adi(
      geography = "state",
      year = 2009,
      dataset = "acs1",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # set7
  expect_snapshot(
    get_adi(
      geography = "state",
      year = 2008,
      dataset = "acs3",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # 2000 decennial
  expect_snapshot(
    get_adi(
      geography = "county",
      state = "NM",
      year = 2000,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  # 2020 decennial
  expect_snapshot(
    get_adi(
      geography = "county",
      state = "NM",
      year = 2020,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )

  skip(
    paste0("1990 decennial data are currently unavailable.",
           "\nWe will reinstate these tests if they are ever restored.")
  )

  expect_snapshot(
    get_adi(
      geography = "county",
      state = "NM",
      year = 1990,
      dataset = "decennial",
      keep_indicators = TRUE,
      cache_tables = FALSE
    )
  )
})
