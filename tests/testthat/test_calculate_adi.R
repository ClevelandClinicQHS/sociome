context("Test calculate_adi()")

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