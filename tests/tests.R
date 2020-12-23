test_that("year function", {
  expect_that(make_filename(2012), is_equivalent_to("accident_2012.csv.bz2"))
})