test_that("year function", {
  expect_that(make_filename(2012), "accident_2012.csv.bz2")
})