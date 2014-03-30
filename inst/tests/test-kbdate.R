
context("kbdate")

test_that("kbdate works", {

  expect_equal(kbdate(), format(Sys.Date(), "%Y-%m-%d"))

})
