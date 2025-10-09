context("crayons picks out the names correctly")

test_that("crayons works", {

    expect_equal(crayons("royal"), c("Royal Purple"="#7851a9"))

    expect_warning(
        expect_equal(crayons(c("royal", "blah")), c("Royal Purple"="#7851a9", blah=NA))
    )
    expect_warning(
        expect_equal(crayons("royal", "blah"), c("Royal Purple"="#7851a9", blah=NA))
    )
    expect_warning(
        expect_equal(crayons(c("blah", "royal")), c(blah=NA, "Royal Purple"="#7851a9"))
    )
    expect_warning(
        expect_equal(crayons("blah", "royal"), c(blah=NA, "Royal Purple"="#7851a9"))
    )

    expect_warning(
        expect_equal(crayons(c("royal", "blah", "a")), c("Royal Purple"="#7851a9", blah=NA, a=NA))
    )
    expect_warning(
        expect_equal(crayons("royal", "blah", "a"), c("Royal Purple"="#7851a9", blah=NA, a=NA))
    )
    expect_warning(
        expect_equal(crayons(c("royal", "blah"), "a"), c("Royal Purple"="#7851a9", blah=NA, a=NA))
    )
    expect_warning(
        expect_equal(crayons("royal", c("blah", "a")), c("Royal Purple"="#7851a9", blah=NA, a=NA))
    )

    expect_warning(
        expect_equal(crayons(c("a", "royal")), c(a=NA, "Royal Purple"="#7851a9"))
    )

    expect_warning(
        expect_equal(crayons("royal", "mountain"), c("Royal Purple"="#7851a9", mountain=NA))
    )

    expect_equal(crayons("royal", "purple mountain"), c("Royal Purple"="#7851a9",
                                                        "Purple Mountain's Majesty"="#9d81ba" ))


    # works with exact matches?
    expect_equal(crayons("Apricot"), c(Apricot="#fdd9b5"))
    expect_equal(crayons("Aprico"), c(Apricot="#fdd9b5"))
    expect_equal(crayons("Apricot", "royal"),
                 c(Apricot="#fdd9b5", "Royal Purple"="#7851a9"))
    expect_equal(crayons("Apricot", "Royal Purple"),
                 c(Apricot="#fdd9b5", "Royal Purple"="#7851a9"))
    expect_equal(crayons("Aprico", "Royal Purple"),
                 c(Apricot="#fdd9b5", "Royal Purple"="#7851a9"))

})
