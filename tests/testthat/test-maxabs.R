context("maxabs")

test_that("maxabs works", {

    input <- c(231.3, 310.2, -2123.8, 991.2, 95.9)
    expect_equal(maxabs(input), max(abs(input)))

    input <- c(231.3, 310.2, -2123.8, NA, 991.2, 95.9)
    expect_equal(maxabs(input), max(abs(input)))

    input <- c(231.3, 310.2, -2123.8, NA, 991.2, 95.9)
    expect_equal(maxabs(input, na.rm=TRUE), max(abs(input), na.rm=TRUE))

})
