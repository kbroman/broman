#' Pick the more precise value for each element in two related vectors
#'
#' Align two vectors of numbers by their names and then pick a single
#' value from each, favoring the more precise one. If the two values
#' differ by more than round-off error, treat the value as missing.
#'
#' @md
#'
#' @param x A numeric vector
#' @param y A second numeric vector
#' @param tol Tolerance for differences between the values
#'
#' @return A vector of combined values
#'
#' @details
#' Okay, this is a bit weird. But suppose you have two columns of
#' numbers that have been subjected to different quirky rounding
#' patterns. We align the vectors using their names and then for each
#' element we pick between the two choices, favoring the more-precise
#' one. If one is missing, choose the non-missing value. If the two
#' differ by more than the round-off error, treat it as missing.
#'
#' @export
pick_more_precise <-
    function(x, y, tol=1e-6)
{
    # turn tol into integer
    tol <- floor(-log10(tol))

    xn <- names(x)
    yn <- names(y)

    if(is.null(xn) || is.null(yn)) { # can't align them
        if(length(x) != length(y))
            stop("length(x) != length(y) and at least one has missing names")

        if(is.null(xn)) names(x) <- yn
        if(is.null(yn)) names(y) <- xn
    }
    else {
        # align the two vectors using their names attributes
        #     pad with NAs as necessary
        aligned <- align_vectors(x,y, expand=TRUE)
        x <- aligned$x
        y <- aligned$y
    }

    # paste in x (with names)
    result <- x

    # get number of digits in each
    dx <- get_precision(x)
    dy <- get_precision(y)

    n_made_na <- 0
    for(i in seq_along(x)) {
        if(is.na(x[i]) || is.na(y[i])) { # one or the other is missing
            # keep the non-missing value, if there is one
            if(is.na(x[i])) result[i] <- y[i]
            if(is.na(y[i])) result[i] <- x[i]
        } else {
            # are the results more different than from rounding?
            d <- abs(round(x[i], dy[i]) - round(y[i], dx[i]))
            if(d > 0 && floor(-log10(d)) < min(c(tol, dx[i], dy[i])+1)) {
                n_made_na <- n_made_na + 1
                result[i] <- NA
            }
            else if(dy[i] > dx[i]) { # pick the y value if it's more precise
                result[i] <- y[i]
            }
        }
    }

    if(n_made_na > 0) { # issue warning if some values were omitted
        warning("Omitted ", n_made_na,
                ifelse(n_made_na==1, " value ", " values "),
                "due to large differences between x and y")
    }

    result

}
