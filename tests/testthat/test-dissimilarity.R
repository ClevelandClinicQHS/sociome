test_that("append_dissimilarities() works", {
  expect_snapshot(append_dissimilarities(mtcars))

  expect_snapshot(append_dissimilarities(mtcars, cols = !c(wt, qsec)))
})
