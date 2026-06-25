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


test_that("jiggle with method=random works", {

    set.seed(20260625)

    g <- c(CC004 = NA, CC007 = "H", CC011 = "H", CC012 = "B", CC015 = "D",
           CC017 = "A", CC020 = NA, CC023 = "B", CC025 = "E", CC029 = "E",
           CC032 = "A", CC037 = "A", CC040 = "E", CC041 = "D", CC058 = "D",
           CC059 = "B", CC061 = "A", CC068 = "B", CC078 = "C", CC080 = NA)
    y <- c(CC004 = 5.5833, CC007 = 11.9, CC011 = 11.6, CC012 = 8.7333,
           CC015 = 5.55, CC017 = 6.8143, CC020 = 4.8, CC023 = 9.85, CC025 = 7.75,
           CC029 = 8.95, CC032 = 6.9, CC037 = 5.04, CC040 = 7.375, CC041 = 8.1778,
           CC058 = 6.8333, CC059 = 8.95, CC061 = 6.8444, CC068 = 9.7333,
           CC078 = 4.6, CC080 = 6.4143)

    expected <- c(0.00303764961194247, 0, 0, 0, 0, 0.04263560653897, 0.0536223826871719,
                  0.00680270611541346, 0, 0, -0.0347004210250452, 0, 0, 0, 0, 0,
                  0.0348853355948813, -0.0128668125439435, 0, -0.0614784541307017)
    expect_equivalent(jiggle(g, y, method="random", maxvalue=NULL), expected)

    expected <- c(0.00327947095967829, 0, 0, 0, 0, -0.00282955850474536, 0.0221720405714586,
                  0.00433999395230785, 0, 0, -0.00424579955171794, 0, 0, 0, 0,
                  0, 0.00957026235759258, 0.00199242285918444, 0, 0.0977408314589411)
    expect_equivalent(jiggle(g, y, method="random", maxvalue=0.1), expected)

    m <- c(CC004 = "D", CC008 = "H", CC015 = "D", CC017 = "G", CC020 = "B",
           CC023 = NA, CC025 = "E", CC029 = "C", CC032 = "C", CC037 = "D",
           CC058 = "F", CC061 = "G", CC080 = "A")
    y <- c(CC004 = -2.8167, CC008 = -5.925, CC015 = -2.38, CC017 = -2.7189,
           CC020 = -1.82, CC023 = -3.5, CC025 = -2.5, CC029 = 0.9, CC032 = 0.8,
           CC037 = -2.675, CC058 = -0.975, CC061 = -2.4743, CC080 = -2.54)

    expected <- c(-0.0513827899703756, 0, 0, 0, 0, -0.163120478927158, 0, -0.0521027790033258,
                  0.0196616823901422, 0.0682791111990809, 0, 0, 0)
    expect_equivalent(jiggle(m, y, method="random"), expected)

    expected <- c(0.0137072402130192, 0, 0, 0, 0, -0.00972040663473309, 0, -0.0141159515672674,
                  -0.00592605986942848, -0.0041690049925819, 0, 0, 0)
    expect_equivalent(jiggle(m, y, method="random", maxvalue=0.1), expected)

})
