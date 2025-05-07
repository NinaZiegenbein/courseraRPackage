library(testthat)
library(courseraRPackage)

test_that("fars_read reads a file correctly", {
  path <- system.file("extdata", "accident_2013.csv.bz2", package = "fars")
  expect_s3_class(fars_read(path), "tbl_df")
})

test_that("make_filename returns a correctly formatted filename", {
  expect_equal(make_filename(2015), "accident_2015.csv.bz2")
})

test_that("fars_read_years returns list of tibbles or NULL", {
  result <- fars_read_years(c(2013, 2014))
  expect_true(all(sapply(result, function(x) is.null(x) || inherits(x, "tbl_df"))))
})

test_that("fars_summarize_years returns a wide summary", {
  result <- fars_summarize_years(c(2013, 2014))
  expect_s3_class(result, "tbl_df")
  expect_true("MONTH" %in% names(result))
})

