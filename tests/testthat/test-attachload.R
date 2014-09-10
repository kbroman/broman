
context("attachfile")

test_that("attachfile and loadfile work", {

    # create a file
    z <- savedz <- "blah"
    number <- 4
    file <- paste0("perm000", number, ".RData")
    save(z, file=file)
    rm(z)

    loadfile(number, verbose=TRUE)
    expect_equal(z, savedz)
    rm(z, envir=.GlobalEnv)

    attachfile(number)
    expect_equal(z, savedz)

    detach(2)
    unlink(file)

})

test_that("attachfile and loadfile work with a different filename", {

    # create a file
    z <- savedz <- "blah"
    number <- 4
    stem <- "blah"
    end <- "_end.RData"
    file <- paste0(stem, "000", number, end)
    save(z, file=file)
    rm(z)

    loadfile(number, stem, end)
    expect_equal(z, savedz)
    rm(z, envir=.GlobalEnv)

    attachfile(number, stem, end)
    expect_equal(z, savedz)

    detach(2)
    unlink(file)
})

