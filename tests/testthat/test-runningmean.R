
context("running mean")

test_that("running mean stops when it should", {

  expect_error( runningmean(0, c(0,0)) )

})


test_that("running mean with constant x or position", {

  n <- 100
  x <- rnorm(n)
  pos <- rep(0, n)

  expect_equal( runningmean(pos, x, window=1), rep(mean(x), n) )
  expect_equal( runningmean(pos, x, window=1, what="median"), rep(median(x), n) )
  expect_equal( runningmean(pos, x, window=1, what="sum"), rep(sum(x), n) )
  expect_equal( runningmean(pos, x, window=1, what="sd"), rep(sd(x), n) )

  mu <- mean(x)
  x <- rep(mu, n)
  pos <- runif(n, 0, 5)

  expect_equal( runningmean(pos, x, window=1), x)
  expect_equal( runningmean(pos, x, window=1, what="median"), x)
  expect_equal( runningmean(pos, x, window=5, what="sd"), rep(0, n))

})
