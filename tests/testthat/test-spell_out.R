context("spell_out")

test_that("spell_out() works", {

    expect_equal(spell_out(9), "nine")
    expect_equal(spell_out(9, capitalize=TRUE), "Nine")
    expect_equal(spell_out(9, max_value=8), "9")

    expect_equal(spell_out(0), "zero")
    expect_equal(spell_out(0, capitalize=TRUE), "Zero")

    expect_equal(spell_out(-1), "-1")

    expect_equal(spell_out(21), "21")
    expect_equal(spell_out(20), "20")
    expect_equal(spell_out(20, max_value=20), "twenty")

    expect_warning(expect_equal(spell_out(21, max_value=21), "21"))
    expect_warning(expect_equal(spell_out(7.2), "seven"))
    expect_warning(expect_equal(spell_out(6.8), "seven"))

    expect_equal(spell_out(1:5), numbers[1:5])
    expect_equal(spell_out(5:1, capitalize=TRUE), Numbers[5:1])

})
