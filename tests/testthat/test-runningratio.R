context("running ratio")

test_that("running ratio stops when it should", {

  expect_error( runningratio(c(0,1), c(0,0), 1) )
  expect_error( runningratio(c(0,1), 0, c(1,1)) )
  expect_error( runningratio(0, c(0,0), c(1,1)) )
  expect_error( runningratio(0, c(0,0), c(1,1,2)) )

})


test_that("running ratio with constant x or position", {

  n <- 100
  d <- 5
  x <- rnorm(n)
  pos <- rep(0, n)
  denom <- rep(d, n)

  expect_equal( runningratio(pos, x, denom, window=1), rep(mean(x)/d, n) )

  mu <- mean(x)
  x <- rep(mu, n)
  pos <- runif(n, 0, 5)

  expect_equal( runningratio(pos, x, denom, window=1), x/d)

})
