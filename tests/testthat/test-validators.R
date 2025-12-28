test_that("validate_geography() works", {
  expect_equal(validate_geography("zcta"), "zip code tabulation area")
})

test_that("validate_single_positive_integer() works", {
  expect_error(validate_single_positive_integer(Inf, "foo"), "single positive integer")
  expect_error(validate_single_positive_integer(0, "foo"), "single positive integer")
  expect_equal(validate_single_positive_integer(3, "foo"), 3L)
})

test_that("validate_year() works", {
  expect_error(validate_year(2011, c(2000, 2010, 2020)), "year must be one of")
  expect_equal(validate_year(2020, c(2000, 2010, 2020)), 2020L)
})

test_that("validate_dataset() works", {
  expect_error(validate_dataset("decennial", 2011), "year must be 2000, 2010, or 2020")
  expect_error(validate_dataset("decennial", 2000, "zip code tabulation area"), '"zip code tabulation area" not supported')
  expect_warning(validate_dataset("acs5", 2015, "block group"), "Median family income")
})

test_that("determine_input_arg() works", {
  expect_equal(determine_input_arg(), "NULL")
  expect_equal(determine_input_arg(state = "OH"), "state")
  expect_error(determine_input_arg(county = "Cuyahoga"), "Can't enter county when state is NULL")
  expect_equal(determine_input_arg(geoid = "39035"), "geoid")
  expect_equal(determine_input_arg(state = "Ohio", county = "Cuyahoga"), "county")
  expect_error(determine_input_arg(state = "Ohio", geoid = "39035"), "Enter only one of")
  expect_error(determine_input_arg(county = "Cuyahoga", geoid = "39035"), "Enter only one of")
  expect_error(determine_input_arg(state = "Ohio", county = "Cuyahoga", geoid = "39035"), "Enter only one of")
})

test_that("validate_state() works", {
  expect_warning(state_from_county_geoid <- validate_state("39035"), "More than two digits")
  expect_equal(state_from_county_geoid, "39")
  expect_equal(validate_state("oh"), "39")
  expect_equal(validate_state("ohIo"), "39")
  expect_error(validate_state("99"), "does not match any state")
  expect_error(validate_state("YZ"), "does not match any state")
  expect_error(validate_state("canada"), "does not match any state")
})

test_that("validate_county() works", {
  expect_error(validate_county("ohio", "99035"), "The first two digits of county.+do not match.+state")
  expect_error(validate_county("ohio", "39999"), "does not match any counties in state")
  expect_error(validate_county("texas", "moNt"), "matches more than one county in state")
  expect_equal(validate_county("ohio", "cuYahO"), "39035")
})




test_that("validate_single_geoid() works", {
  expect_equal(validate_single_geoid("12"), "12")
  expect_error(validate_single_geoid("123"), "geoid must be a single string consisting of")
  expect_equal(validate_single_geoid("12345"), "12345")
  expect_equal(validate_single_geoid("12345678901"), "12345678901")
  expect_equal(validate_single_geoid("123456789012"), "123456789012")
})


test_that("validate_geoid() works", {
  expect_error(validate_geoid(character()), "Each element of geoid must have exactly")
  expect_error(validate_geoid(c(" 39", NA)), "Each element of geoid must have exactly")
  expect_error(validate_geoid(c(" 39", "  39035  ", "a ")), "Each element of geoid must have exactly")
  expect_equal(validate_geoid(c("01  ", "12345", "12345678901", "123456789012")), c("01", "12345", "12345678901", "123456789012"))
})

test_that("validate_single_string() works", {
  expect_null(validate_single_string(NULL, null_ok = TRUE))
  expect_error(validate_single_string(1, "foo"), "foo must be a single nonempty")
  expect_error(validate_single_string("", "foo"), "foo must be a single nonempty")
  expect_equal(validate_single_string("foo"), "foo")
})


test_that("validate_geo_length()", {
  expect_error(validate_geo_length("foo"), "geography does not match any of")
  expect_warning(foo <- validate_geo_length("state", "39035"), "geoids are more granular than geography")
  expect_equal(foo, 2L)
})



test_that("validate_lon_lat() works", {
  expect_error(validate_lon_lat(c(1, Inf)), "center must be coercible to a double vector of length 2")
  expect_equal(validate_lon_lat(c(T, F)), 1:0)
})


test_that("validate_dissim_colnames() works", {
  expect_error(validate_dissim_colnames(NULL, NULL, mtcars), "Can't make both .+ NULL")
  expect_error(validate_dissim_colnames("", "foo", mtcars), "must be a single, nonmissing, nonempty string")
  expect_error(validate_dissim_colnames("mpg", "foo", data = mtcars), "A column named mpg already exists")
  expect_error(validate_dissim_colnames("foo", "", mtcars), "must be a single, nonmissing, nonempty string")
  expect_error(validate_dissim_colnames("foo", "mpg", mtcars), "A column named mpg already exists")
  expect_null(validate_dissim_colnames("foo", "bar", mtcars))
})

