context("cf")

test_that("cf works", {

    x <- c(5, 8, 9, NA, 3, NA)
    y <- c(5, 2, 9, 4, NA, NA)

    expected <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
    expect_equal( cf(x,y), expected )
    expect_equal( cf(x,y, tol=1e-6), expected )
    expect_equal( cf(sqrt(x),sqrt(y), tol=1e-6), expected )

    a <- list(x, sqrt(x))
    b <- list(y, sqrt(y))

    expect_equal( cf(a, b), list(expected, expected) )
    expect_equal( cf(a, b, tol=1e-6), list(expected, expected) )

})
