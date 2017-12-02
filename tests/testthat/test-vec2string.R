context("vec2string")

test_that("vec2string works", {

    expect_equal(vec2string(NULL), "")
    expect_equal(vec2string("a"), "a")
    expect_equal(vec2string(c("a", "b")), "a and b")
    expect_equal(vec2string(c("a", "b", "c", "d")), "a, b, c, and d")

})
