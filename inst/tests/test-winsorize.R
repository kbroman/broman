
context("winsorize")

test_that("winsorize works for small vectors", {

  input <-   c(2, 3, 7, 9, 6, NA, 5, 8, NA, 0, 4, 1, 10)
  result1 <- c(2, 3, 7, 9, 6, NA, 5, 8, NA, 1, 4, 1,  9)
  result2 <- c(2, 3, 7, 8, 6, NA, 5, 8, NA, 2, 4, 2,  8)
  expect_identical(winsorize(input, 0.1), result1)
  expect_identical(winsorize(input, 0.2), result2)

})

test_that("winsorize works for a long vector", {

  set.seed(94745689)
  n <- 1000
  nmis <- 10
  p <- 0.05
  input <- rnorm(n)
  input[sample(1:n, nmis)] <- NA
  quL <- quantile(input, p, na.rm=TRUE)
  quH <- quantile(input, 1-p, na.rm=TRUE)

  result <- winsorize(input, p)
  expect_identical(is.na(input), is.na(result))
  middle <- !is.na(input) & input >= quL & input <= quH
  low <- !is.na(input) & input <= quL
  high <- !is.na(input) & input >= quH
  expect_identical(input[middle], result[middle])
  expect_true( all(result[low] == quL) )
  expect_true( all(result[high] == quH) )

})
