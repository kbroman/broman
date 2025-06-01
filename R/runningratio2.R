######################################################################
# runningratio2
#
# take sum(numerator)/sum(denominator) in sliding window,
# but rather than a fixed use nearest positions to give some target denominator
######################################################################
#  runningratio2
#'
#' Running ratio with adaptive window
#'
#' Calculates a running ratio; a ratio sum(top)/sum(bottom) in a sliding window,
#' but rather than a fixed-width window, use nearest positions to give some
#' target denominator
#'
#' @param pos Positions for the values.
#'
#' @param numerator Values for numerator in ratio.
#'
#' @param denominator Values for denominator in ratio.
#'
#' @param at Positions at which running ratio is
#' calculated.  If NULL, `pos` is used.
#'
#' @param window_denom Target denominator for window for calculating ratio
#'
#' @useDynLib broman, .registration=TRUE
#' @export
#' @return
#' A vector with the same length as the input `at` (or `pos`,
#'   if `at` is NULL), containing the running ratio.
#'
#' @author
#' Karl W Broman \email{broman@@wisc.edu}
#'
#' @examples
#' x <- 1:1000
#' y <- runif(1000, 1, 5)
#' z <- runif(1000, 1, 5)
#' plot(x, runningratio2(x, y, z, window_denom=10), type="l", lwd=2)
#' lines(x, runningratio2(x, y, z, window_denom=50), lwd=2, col="blue")
#' lines(x, runningratio2(x, y, z, window_denom=100), lwd=2, col="red")
#'
#' @seealso [runningmean()], [runningratio()]
#'
#' @keywords
#' univar
runningratio2 <-
    function(pos, numerator, denominator, at=NULL, window_denom=100)
{
    n <- length(pos)
    if(length(numerator) != n || length(denominator) != n)
        stop("pos, numerator and denominator must all be the same length\n")

    if(is.null(at)) { # if missing 'at', use input 'pos'
        at <- pos[!is.na(pos)]
    }

    omit <- (is.na(pos) | is.na(numerator) | is.na(denominator))
    if(any(omit)) {
        pos <- pos[!omit]
        denominator <- denominator[!omit]
        numerator <- numerator[!omit]
    }


    # check that pos is sorted
    if(any(diff(pos) < 0)) { # needs to be sorted
        o <- order(pos)
        pos <- pos[o]
        denominator <- denominator[o]
        numerator <- numerator[o]
    }

    # check that at is sorted
    if(any(diff(at) < 0)) { # needs to be sorted
        o.at <- order(at)
        at <- at[o.at]
        reorderresult <- TRUE
    }
    else reorderresult <- FALSE

    n.res <- length(at)

    z <- .C("R_runningratio2",
            as.integer(n),
            as.double(pos),
            as.double(numerator),
            as.double(denominator),
            as.integer(n.res),
            as.double(at),
            z=as.double(rep(0,n.res)),
            as.double(window_denom),
            PACKAGE="broman")$z

    if(reorderresult)
        z <- z[match(1:length(z), o.at)]

    z
}
