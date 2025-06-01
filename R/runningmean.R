######################################################################
# get running mean, sum or median within a specified window
######################################################################
#  runningmean
#'
#' Running mean, sum, or median
#'
#' Calculates a running mean, sum or median with a specified window.
#'
#' @param pos
#' Positions for the values.
#'
#' @param value
#' Values for which the running mean/sum/median/sd is to be
#'    applied.
#'
#' @param at Positions at which running mean (or sum or median or sd) is
#' calculated.  If NULL, `pos` is used.
#'
#' @param window  Window width.
#'
#' @param what  Statistic to use.
#'
#' @useDynLib broman, .registration=TRUE
#' @export
#' @return
#' A vector with the same length as the input `at` (or `pos`,
#'   if `at` is NULL), containing the running
#'   statistic.
#'
#' @author
#' Karl W Broman \email{broman@@wisc.edu}
#'
#' @examples
#' x <- 1:10000
#' y <- rnorm(length(x))
#' plot(x,y, xaxs="i", yaxs="i")
#' lines(x, runningmean(x, y, window=100, what="mean"),
#'       col="blue", lwd=2)
#' lines(x, runningmean(x, y, window=100, what="median"),
#'       col="red", lwd=2)
#' lines(x, runningmean(x, y, window=100, what="sd"),
#'       col="green", lwd=2)
#'
#' @seealso [runningratio(), runningratio2()]
#'
#' @keywords
#' univar
runningmean <-
    function(pos, value, at=NULL, window=1000, what=c("mean","sum", "median", "sd"))
{
    what <- which(c("sum","mean","median","sd")==match.arg(what))

    n <- length(pos)
    if(length(value) != n)
        stop("pos and value must have the same length\n")

    if(is.null(at)) { # if missing 'at', use input 'pos'
        at <- pos[!is.na(pos)]
    }

    omit <- (is.na(pos) | is.na(value))
    if(any(omit)) {
        pos <- pos[!omit]
        value <- value[!omit]
    }

    # check that pos is sorted
    if(any(diff(pos) < 0)) { # needs to be sorted
        o <- order(pos)
        pos <- pos[o]
        value <- value[o]
    }

    # check that at is sorted
    if(any(diff(at) < 0)) { # needs to be sorted
        o.at <- order(at)
        at <- at[o.at]
        reorderresult <- TRUE
    }
    else reorderresult <- FALSE

    n.res <- length(at)

    z <- .C("R_runningmean",
            as.integer(n),
            as.double(pos),
            as.double(value),
            as.integer(n.res),
            as.double(at),
            z=as.double(rep(0,n.res)),
            as.double(window),
            as.integer(what),
            PACKAGE="broman")$z

    if(reorderresult)
        z <- z[match(1:length(at), o.at)]

    z
}


######################################################################
# runningratio
#
# take sum(numerator)/sum(denominator) in sliding window
######################################################################
#  runningratio
#'
#' Running ratio
#'
#' Calculates a running ratio; a ratio sum(top)/sum(bottom) in a sliding window.
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
#' @param window Window width.
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
#' plot(x, runningratio(x, y, z, window=5), type="l", lwd=2)
#' lines(x, runningratio(x, y, z, window=50), lwd=2, col="blue")
#' lines(x, runningratio(x, y, z, window=100), lwd=2, col="red")
#'
#' @seealso [runningmean(), runningratio2()]
#'
#' @keywords
#' univar
runningratio <-
    function(pos, numerator, denominator, at=NULL, window=1000)
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

    z <- .C("R_runningratio",
            as.integer(n),
            as.double(pos),
            as.double(numerator),
            as.double(denominator),
            as.integer(n.res),
            as.double(at),
            z=as.double(rep(0,n.res)),
            as.double(window),
            PACKAGE="broman")$z

    if(reorderresult)
        z <- z[match(1:length(z), o.at)]

    z
}
