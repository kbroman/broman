#  dotplot
#'
#' Dot chart with a gray background
#'
#' Like the \code{\link{grayplot}} function, but with one axis assumed to be a group.
#'
#' @param group Categorical coordinates for the plot
#' @param y Coordinates of points in the plot
#'
#' @param rotate If TRUE, have group as y-axis; default (FALSE) has
#' group on x-axis.
#'
#' @param ... Optional graphics arguments
#'
#'
#' @details Calls \code{\link{grayplot}} with special choices of
#' graphics parameters for the case of categorical x.
#'
#' @export
#' @importFrom graphics axis
#'
#' @return
#' None.
#'
#' @examples
#' \dontshow{set.seed(97536917)}
#' x <- rnorm(40, c(1,3))
#' g <- rep(c("control", "treatment"), 20)
#' dotplot(g, x, pch=21, bg="slateblue")
#'
#' @seealso
#' \code{\link[graphics]{grayplot}},
#'
#' @keywords
#' graphics
dotplot <-
    function(group, y, rotate=FALSE, ...)
{
    stopifnot(length(y) == length(group))
    if(length(unique(y)) < length(unique(group)))
        warning('Seems like maybe "group" and "y" got switched.')

    # turn group into numbers 1, 2, ..., n_group
    if(is.factor(group)) {
        ugroup <- levels(group)
        group <- as.numeric(ugroup)
    }
    else {
        ugroup <- unique(group)
        group <- match(group, ugroup)
    }
    n_group <- length(ugroup)
    jit <- runif(length(y), -0.25, 0.25)

    # this is to deal with varying inputs
    hidedotplot <-
        function(group, y, rotate=FALSE,
                 vlines=NULL, vlines.col="white", vlines.lwd=1,
                 hlines=NULL, hlines.col="white", hlines.lwd=1,
                 xat=NULL, xlim=NULL, xaxs="r", xlab=NULL,
                 yat=NULL, ylim=NULL, yaxs="r", ylab=NULL,
                 las=1,
                 ...)

        {
            if(!rotate) {
                xlim <- c(0.5, n_group+0.5)
                vlines <- 1:n_group
                vlines.col <- "gray70"
                vlines.lwd <- 4
                xat <- NA
                if(is.null(xlab)) xlab <- "Group"

                # deal with vlines/lines ** FIX ME **

                grayplot(group+jit, y,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab, las=las, ...)
                axis(side=1, at=vlines, ugroup, las=las)

            }
            else {
                ylim <- c(0.5, n_group+0.5)
                hlines <- 1:n_group
                hlines.col <- "gray70"
                hlines.lwd <- 4
                yat <- NA
                if(is.null(ylab)) ylab <- "Group"

                grayplot(y, group + jit,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab,
                         v_over_h=TRUE, las=las, ...)
                axis(side=2, at=hlines, ugroup, las=las)
            }

        }

    hidedotplot(group, y, rotate, ...)
    invisible()
}
