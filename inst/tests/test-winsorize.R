
context("winsorise")

test_that("winsorize works for small vectors", {

  x <-  c(2, 3, 7, 9, 6, NA, 5, 8, NA, 0, 4, 1, 10)
  y1 <- c(2, 3, 7, 9, 6, NA, 5, 8, NA, 1, 4, 1,  9)
  y2 <- c(2, 3, 7, 8, 6, NA, 5, 8, NA, 2, 4, 2,  8)
  expect_identical(winsorize(x, 0.1), y1)
  expect_identical(winsorize(x, 0.2), y2)

})
