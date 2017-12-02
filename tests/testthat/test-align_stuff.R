context("Align stuff")

test_that("align_vectors works", {

    # a few in common
    x <- c(A=1, B=2, C=3)
    y <- c(B=5, C=6, D=7, E=8)

    expect_equal(align_vectors(x, y),
                 list(x=c(A=1, B=2, C=3, D=NA, E=NA), y=c(A=NA, B=5, C=6, D=7, E=8)))

    expect_equal(align_vectors(x, y, expand=FALSE),
                 list(x=c(B=2, C=3), y=c(B=5, C=6)))

    # nothing in common
    x <- c(A=1, B=2, C=3)
    y <- c(E=5, F=6, G=7, H=8)

    expect_equal(align_vectors(x, y),
                 list(x=c(A=1, B=2, C=3, E=NA, F=NA, G=NA, H=NA),
                      y=c(A=NA, B=NA, C=NA, E=5, F=6, G=7, H=8)))

    expect_equal(align_vectors(x, y, expand=FALSE),
                 list(x=setNames(numeric(0), character(0)),
                      y=setNames(numeric(0), character(0))))


    # all in common, just different order
    x <- c(A=1, B=2, C=3)
    y <- c(C=5, A=6, B=7)

    expect_equal(align_vectors(x, y),
                 list(x=c(A=1, B=2, C=3), y=c(A=6, B=7, C=5)))
    expect_equal(align_vectors(x, y, expand=FALSE),
                 list(x=c(A=1, B=2, C=3), y=c(A=6, B=7, C=5)))

})
