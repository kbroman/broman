context("Pick the more precise values from two aligned vectors")

test_that("get_more_precise() works", {

    set.seed(20171202)

    # two random vectors
    x <- y <- rnorm(10)

    # round some of each
    y[1:5] <- round(y[1:5], 3)
    x[7:10] <- round(x[7:10], 5)

    expected <- c(x[1:6], y[7:10])

    expect_equal(pick_more_precise(x,y), expected)

    # different sizes without names: should give an error
    expect_error(pick_more_precise(x, y[1:8]))

    # include names and re-order y
    names(x) <- names(y) <- LETTERS[1:10]
    y <- y[sample(1:10)]

    expect_equal(pick_more_precise(x,y), setNames(expected, LETTERS[1:10]))

})
