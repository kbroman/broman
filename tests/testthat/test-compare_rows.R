context("compare_rows")

test_that("compare_rows works for a simple case", {

    x <- rbind(c(1,1,1,1,1), c(2,2,2,2,2), c(1,1,2,2,2))

    expected <- rbind(c(NA, 1, 0.6),
                      c(1, NA, 0.4),
                      c(0.6, 0.4, NA))
    dimnames(expected) <- list(NULL, NULL)
    expect_equal(compare_rows(x), expected)

    expected <- rbind(c(NA, 1, sqrt(3/5)),
                      c(1, NA, sqrt(2/5)),
                      c(sqrt(3/5), sqrt(2/5), NA))
    dimnames(expected) <- list(NULL, NULL)
    expect_equal(compare_rows(x, "rms"), expected)

})

test_that("compare_rows works with some NAs", {

    x <- rbind(c(NA,1,1,1,1), c(2,NA,2,2,2), c(1,1,NA,2,2))
    rownames(x) <- LETTERS[1:3]

    expected <- rbind(c(NA, 1, 2/3),
                      c(1, NA, 1/3),
                      c(2/3, 1/3, NA))
    dimnames(expected) <- list(LETTERS[1:3], LETTERS[1:3])
    expect_equal(compare_rows(x), expected)

    expected <- rbind(c(NA, 1, sqrt(2/3)),
                      c(1, NA, sqrt(1/3)),
                      c(sqrt(2/3), sqrt(1/3), NA))
    dimnames(expected) <- list(LETTERS[1:3], LETTERS[1:3])
    expect_equal(compare_rows(x, "rms"), expected)

})
