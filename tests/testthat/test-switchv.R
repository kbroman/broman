context("vectorized switch")

test_that("switchv works for strings", {

    result <- switchv(c("horse", "fish", "cat", "bug"),
                      horse="fast",
                      cat="cute",
                      "what?")

    expect_equal(result, c("fast", "what?", "cute", "what?"))


    result <- switchv(c("horse", "fish", "cat", "bug"),
                      horse="fast",
                      cat="cute",
                      bug="ugly",
                      fish="wet")

    expect_equal(result, c("fast", "wet", "cute", "ugly"))

})
