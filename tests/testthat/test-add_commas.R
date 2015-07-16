context("add_commas")

test_that("add_commas works", {

    input <- c(231.3, 91310.2, 2123.8, 9911001020, 999723285)
    output <- c("231.3", "91,310.2", "2,123.8", "9,911,001,020.0", "999,723,285.0")
    expect_equal(add_commas(input), output)

    output <- c("231", "91,310", "2,124", "9,911,001,020", "999,723,285")
    expect_equal(add_commas(round(input)), output)

})
