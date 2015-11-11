#  histlines

# Either:
# x = breaks for histogram; y = counts or density
#
# or:
# x = data, breaks=pass to hist()

#'
#' Utility to create line-based histogram
#'
#' Utility function to plot histogram with \code{\link[graphics]{lines}}.
#'
#' @param x Either vector of breaks or the data itself.
#'
#' @param y Optional vector of density/counts, with length = \code{length(x)-1}.
#'
#' @param breaks Breaks for histogram, if \code{y} is not provided.
#'
#' @param use Whether to use \code{counts} or \code{density}, if \code{y}
#'  is not provided.
#'
#' @details
#' If \code{x} and \code{y} are both provided, \code{x} is interpreted to
#'   be the breaks for a histogram, and \code{y} is a vector of counts or
#'   density values for each interval.  These are then revised so that they
#'   may be plotted with \code{\link[graphics]{lines}}.
#'   If \code{y} is missing, \code{x} is taken to be the data.  In this
#'   case \code{\link[graphics]{hist}} is called with \code{breaks=breaks}, and
#'   either the \code{counts} or \code{density} are used as \code{y}.
#'
#' @importFrom graphics hist
#' @export
#' @return
#' A data.frame with two columns: \code{x} and \code{y}.
#'
#' @examples
#' x <- rnorm(1000, mean=20, sd=5)
#' # basic use
#' out <- hist(x, breaks=60, plot=FALSE)
#' plot(histlines(out$breaks, out$counts),
#'      type="l", lwd=2, xlab="x", ylab="counts", las=1)
#' # alternative use
#' plot(histlines(x, breaks=60, use="density"),
#'      type="l", lwd=2, xlab="x", ylab="Density", las=1)
#' # comparing two distributions
#' z <- rnorm(1000, mean=25, sd=5)
#' br <- seq(min(c(x,z)), max(c(x,z)), len=50)
#' xlines <- histlines(x, breaks=br, use="density")
#' zlines <- histlines(z, breaks=br, use="density")
#' ymx <- max(c(xlines$y, zlines$y))*1.05
#' plot(xlines, ylim=c(0, ymx), yaxs="i", xaxs="i",
#'      type="l", lwd=2, xlab="x", ylab="Density", las=1,
#'      col="blue")
#' lines(zlines, lwd=2 , col="red")
#'
#' @seealso
#' \code{\link[graphics]{hist}},
#'   \code{\link[graphics]{lines}}
#'
#' @keywords
#' graphics
histlines <-
    function(x, y, breaks, use=c("counts", "density"))
{
    if(missing(y)) { # input doesn't count the count information
        out <- hist(x, breaks=breaks, plot=FALSE)
        x <- out$breaks
        use <- match.arg(use)
        y <- out[[use]]
    }

    if(length(x) != length(y)+1)
        stop("length(x) != length(y) + 1")

    x <- as.numeric(rbind(x, x))
    y <- c(0, as.numeric(rbind(y,y)), 0)
    data.frame(x=x, y=y)
}
