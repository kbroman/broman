context("myround")

test_that("myround works", {

    expect_equal( myround(0.24, 1), "0.2")
    expect_equal( myround(0.26, 1), "0.3")

    expect_equal( myround(51.01, 3), "51.010" )
    expect_equal( myround(51.01, 1), "51.0" )

    expect_equal( myround(0.199, 2), "0.20" )

    expect_equal( myround(-0.01, 1), "0.0"  )

    expect_equal( myround(-0.51, 1), "-0.5"  )
    expect_equal( myround(-0.56, 1), "-0.6"  )

})
