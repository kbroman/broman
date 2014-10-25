context("length unique")

test_that("lenuniq works", {

    x <- c(1, 2, 1, 3, 1, 1, 2, 2, 3, NA, NA, 1)
    expect_equal(lenuniq(x), 3)
    expect_equal(lenuniq(x, na.rm=FALSE), 4)

    expect_equal(lenuniq(NULL), 0)
    expect_equal(lenuniq(character(0)), 0)

})
