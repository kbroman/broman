#  manyboxplot
#'
#' Boxplot-like figure for many groups
#'
#' Boxplot-like figure for many groups, with lines connecting
#'   selected quantiles.
#'
#' @param x Matrix of data, with columns indicating the groups.
#'
#' @param probs Numeric vecotr of probabilities with values in [0,1).
#'     Quantiles will be symmetric, and the median will always be included.
#'
#' @param dotcol Color for median
#'
#' @param linecol Line colors, same length as \code{probs}
#'
#' @param ... Additional graphics parameters
#'
#' @details
#' Calculates quantiles of the columns of \code{x} and then plots dots or
#'   lines at median plus lines at a series of quantiles, using
#'   \code{\link{grayplot}} for the actual plot.
#'
#' @export
#' @return
#' None.
#'
#' @examples
#' \dontshow{set.seed(8422668)}
#' mu <- c(rnorm(50, 0, 0.3), rnorm(50, 2, 0.3)) # vector of means
#' x <- t(matrix(rnorm(1000*100, mu), ncol=1000))
#' manyboxplot(x, c(0.05, 0.25), ylim=range(x),
#'            dotcol=c("blue","green")[(1:100 > 50) + 1],
#'            hlines=seq(-4, 6, by=2),
#'            vlines=c(1, seq(20, 100, by=20)))
#'
#' @seealso
#' \code{\link{grayplot}}
#'
#' @keywords
#' graphics
manyboxplot <-
    function(x, probs=c(0.05, 0.1, 0.25), dotcol="blue",
             linecol=c("black","red","green", "orange"),
             ...)
{
    if(!all(probs >= 0 & probs < 1/2))
        stop("probs should be >=0 and < 1/2")
    if(length(linecol) < length(probs))
        stop("length(probs) > length(linecol) ", length(probs), " > ", length(linecol))
    p <- ncol(x)
    if(p < 2) stop("ncol(x) should be >= 2")

    probs <- sort(probs)
    probs <- c(probs, 0.5, rev(1-probs))

    xqu <- apply(x, 2, quantile, probs, na.rm=TRUE)

    # this is to deal with varying inputs (did "..." include xaxs or not?)
    manyboxplot.sub <-
        function(xqu, dotcol, xlab="", xaxs="i", ylab="quantiles",
                 xlim=c(0.25, ncol(xqu)+.75), linecol, type="p",
                 xat, lwd=2, lty=1, pch=16, ylim=range(xqu), ...)
        {
            medrow <- (nrow(xqu)-1)/2+1
            ncolx <- ncol(xqu)

            if(missing(xat)) {
                xat <- pretty(1:ncolx)
                xat <- c(1, xat[xat > 0])
            }

            grayplot(1:ncolx, xqu[medrow,], col=dotcol, xlab=xlab, type=type,
                     xlim=xlim, xaxs=xaxs, pch=pch, ylim=ylim, ylab=ylab, xat=xat, ...)
            for(j in 1:(medrow-1)) {
                lines(1:ncolx, xqu[j,], col=linecol[j], lwd=lwd, lty=lty)
                lines(1:ncolx, xqu[nrow(xqu)-j+1,], col=linecol[j], lwd=lwd, lty=lty)
            }
        }

    manyboxplot.sub(xqu, dotcol=dotcol, linecol=linecol, ...)
    invisible()
}
