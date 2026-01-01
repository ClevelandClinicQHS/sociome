test_that("append_dissimilarities() works", {
  expect_error(
    append_dissimilarities(NULL),
    "data must be a data frame with at least one row"
  )
  expect_error(
    append_dissimilarities(dplyr::tibble(a = logical())),
    "data must be a data frame with at least one row"
  )
  expect_error(
    append_dissimilarities(mtcars, dplyr::starts_with("foobar")),
    "cols must specify at least one column in data"
  )
  expect_warning(
    append_dissimilarities(
      dplyr::mutate(mtcars[-8:-9], mpg = c(NA, tail(mpg, -1)))
    ),
    "missing values detected"
  )

  expect_equal(dissim_samp_wts(c(0, 0, 0, 0)), c(0.25, 0.25, 0.25, 0.25))

  expect_snapshot(append_dissimilarities(mtcars))

  expect_snapshot(append_dissimilarities(mtcars, cols = !c(wt, qsec)))
})
