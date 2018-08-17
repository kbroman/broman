context("%nin%, %win%, %wnin%")

test_that("%nin%, %win%, %wnin% work", {

    vals <- c("a", "xa", "b")
    expect_equal( vals %nin% letters, c(FALSE, TRUE, FALSE) )
    expect_equal( vals %win% letters, c("a", "b"))
    expect_equal( vals %wnin% letters, "xa")

    expect_equal( vals %nin% NULL, rep(TRUE, length(vals)) )
    expect_equal( vals %win% NULL, character(0))
    expect_equal( vals %wnin% NULL, vals)

    vals <- c(25, 56.2, 80, -23)
    tab <- c(10, 6.1, 82, 80, 36, -51.4, 56.2, 40, -80, 17)
    expect_equal( vals %nin% tab, c(TRUE, FALSE, FALSE, TRUE) )
    expect_equal( vals %win% tab, c(56.2, 80))
    expect_equal( vals %wnin% tab, c(25, -23))

    expect_equal( vals %nin% NULL, rep(TRUE, length(vals)) )
    expect_equal( vals %win% NULL, numeric(0))
    expect_equal( vals %wnin% NULL, vals)

    expect_equal( NULL %nin% tab, logical(0))
    expect_equal( NULL %win% tab, NULL)
    expect_equal( NULL %wnin% tab, NULL)

})
