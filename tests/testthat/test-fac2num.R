context("fac2num")

test_that("fac2num works", {

    x <- c(3, 4, 9, 4, 9)
    xf <- factor(x, levels=unique(x))
    expect_equal(fac2num(xf), x)

    names(x) <- sample(LETTERS, length(x))
    xf <- factor(x, levels=unique(x))
    expect_equal(fac2num(xf), x)

    z <- c(994.18, 1-1e-6, -25.1, Inf, NA, 1-1e-6)
    zf <- factor(z)
    expect_equal(fac2num(zf), z)

})
