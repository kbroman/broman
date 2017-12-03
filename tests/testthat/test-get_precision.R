context("Get precision of number")

test_that("get_precision() works", {

    a <- c("1512313.", "13.423", "13.42345", "13.1300014134100")

    expect_equal(get_precision(a), c(0, 3, 5, 11))
    expect_equal(get_precision(as.numeric(a)), c(0, 3, 5, 11))

    b <- c("1512313.", "13.423", NA, NA, "13.42345", NA, "13.1300014134100")

    expect_equal(get_precision(b), c(0, 3, NA, NA, 5, NA, 11))
    expect_equal(get_precision(as.numeric(b)), c(0, 3, NA, NA, 5, NA, 11))

})
