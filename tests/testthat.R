library(testthat)
library(fars)

test_that("make_filename(2015) works", {
  expect_equal(make_filename(2015),"accident_2015.csv.bz2")
})
