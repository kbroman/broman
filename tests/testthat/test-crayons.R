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


    # partial and exact matches, with new argument
    expect_equal(crayons("Blue"), setNames("#1f75fe", "Blue"))
    expected <- setNames(c("#7442c8", "#ea7e5d"), c("Purple Heart", "Burnt Sienna"))
    expect_equal(crayons(c("Purple Heart", "Burnt Sienna")), expected)
    expect_equal(crayons("Purple Heart", "Burnt Sienna"), expected)
    expect_equal(crayons(c("Purple Heart", "Burnt Sienna"), notexact=TRUE), expected)
    expect_equal(crayons("Purple Heart", "Burnt Sienna", notexact=TRUE), expected)
    expected_blue <-
                 c(`Blizzard Blue` = "#ace5ee", Blue = "#1f75fe", `Blue Bell` = "#a2a2d0",
                   `Blue Gray` = "#6699cc", `Blue Green` = "#0d98ba", `Blue Violet` = "#7366bd",
                   `Cadet Blue` = "#b0b7c6", `Green Blue` = "#1164b4", `Midnight Blue` = "#1a4876",
                   `Navy Blue` = "#1974d2", `Pacific Blue` = "#1ca9c9", `Robin's Egg Blue` = "#1fcecb",
                   `Sky Blue` = "#80daeb", `Teal Blue` = "#18a7b5", `Turquoise Blue` = "#77dde7",
                   `Violet Blue` = "#324ab2", `Wild Blue Yonder` = "#a2add0")
    expect_equal(crayons("blue", notexact=TRUE), expected_blue)
    expect_equal(crayons("Blue", notexact=TRUE), expected_blue)
    expect_warning(crayons("Purple")) # returns nothing because no exact match
    expected_purple <-
        c(`Purple Heart` = "#7442c8", `Purple Mountain's Majesty` = "#9d81ba",
          `Purple Pizzazz` = "#fe4eda", `Royal Purple` = "#7851a9", `Violet (Purple)` = "#926eae")
    expect_equal(crayons("purple", notexact=TRUE), expected_purple)
    expect_equal(crayons("PURPLE", notexact=TRUE), expected_purple)

})
