context("hex to dec")

test_that("hex2dec and dec2hex work", {

    expect_equal(dec2hex(5), "5")
    expect_equal(dec2hex(15), "f")
    expect_equal(dec2hex(255), "ff")

    expect_equal(hex2dec("5"), 5)
    expect_equal(hex2dec("05"), 5)
    expect_equal(hex2dec("f"), 15)
    expect_equal(hex2dec("0f"), 15)
    expect_equal(hex2dec("ff"), 255)

})
