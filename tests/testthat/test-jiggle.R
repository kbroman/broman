context("jiggle")

test_that("jiggle with method=fixed works", {

    g <- c(CC004 = NA, CC007 = "H", CC011 = "H", CC012 = "B", CC015 = "D",
           CC017 = "A", CC020 = NA, CC023 = "B", CC025 = "E", CC029 = "E",
           CC032 = "A", CC037 = "A", CC040 = "E", CC041 = "D", CC058 = "D",
           CC059 = "B", CC061 = "A", CC068 = "B", CC078 = "C", CC080 = NA)
    y <- c(CC004 = 5.5833, CC007 = 11.9, CC011 = 11.6, CC012 = 8.7333,
           CC015 = 5.55, CC017 = 6.8143, CC020 = 4.8, CC023 = 9.85, CC025 = 7.75,
           CC029 = 8.95, CC032 = 6.9, CC037 = 5.04, CC040 = 7.375, CC041 = 8.1778,
           CC058 = 6.8333, CC059 = 8.95, CC061 = 6.8444, CC068 = 9.7333,
           CC078 = 4.6, CC080 = 6.4143)

    expected <- c(NA, 0, 0, 0, 0, -0.0857142857142857, NA, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0.0857142857142856, 0, 0, NA)
    expect_equivalent(jiggle(g, y, method="fixed", maxvalue=NULL), expected)

    expected <- c(NA, 0, 0, 0, 0, -0.1, NA, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0.1, 0, 0, NA)
    expect_equivalent(jiggle(g, y, method="fixed", maxvalue=0.1), expected)



    m <- c(CC004 = "D", CC008 = "H", CC015 = "D", CC017 = "G", CC020 = "B",
           CC023 = NA, CC025 = "E", CC029 = "C", CC032 = "C", CC037 = "D",
           CC058 = "F", CC061 = "G", CC080 = "A")
    y <- c(CC004 = -2.8167, CC008 = -5.925, CC015 = -2.38, CC017 = -2.7189,
           CC020 = -1.82, CC023 = -3.5, CC025 = -2.5, CC029 = 0.9, CC032 = 0.8,
           CC037 = -2.675, CC058 = -0.975, CC061 = -2.4743, CC080 = -2.54)

    expected <- c(0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0)

    expect_equivalent(jiggle(m, y, method="fixed"), expected)
    expect_equivalent(jiggle(m, y, method="fixed", maxvalue=0.1), expected)


})
