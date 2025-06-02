context("running ratio2")

test_that("runningratio2 stops when it should", {

  expect_error( runningratio2(c(0,1), c(0,0), 1) )
  expect_error( runningratio2(c(0,1), 0, c(1,1)) )
  expect_error( runningratio2(0, c(0,0), c(1,1)) )
  expect_error( runningratio2(0, c(0,0), c(1,1,2)) )

})


test_that("runningratio2 with constant x or position", {

  n <- 100
  d <- 5
  x <- rnorm(n)
  pos <- rep(0, n)
  denom <- rep(d, n)

  expect_equal( runningratio2(pos, x, denom, window_denom=1000), rep(mean(x)/d, n) )

  mu <- mean(x)
  x <- rep(mu, n)
  pos <- runif(n, 0, 5)

  expect_equal( runningratio2(pos, x, denom, window_denom=0.1), x/d)
  expect_equal( runningratio2(pos, x, denom, window_denom=1000), x/d)

})
