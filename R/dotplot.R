#  dotplot
#'
#' Dot chart with a gray background
#'
#' Like the \code{\link{grayplot}} function, but with one axis assumed to be categorical.
#'
#' @param group Categorical coordinates for the plot
#' @param y Coordinates of points in the plot
#'
#' @param rotate If TRUE, have group as y-axis; default (FALSE) has
#' group on x-axis.
#'
#' @param ... Optional graphics arguments
#' @param jiggle Vector of amounts to jiggle the points horizontally,
#' or a character string (\code{"fixed"} or \code{"random"})
#' indicating the jiggling method; see \code{\link{jiggle}}.
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
#' g <- rep(c("A", "B"), 20)
#' dotplot(g, x)
#' dotplot(g, x, "random")
#' dotplot(g, x, runif(length(g), -0.25, 0.25))
#'
#' @seealso
#' \code{\link{grayplot}},
#'
#' @keywords
#' graphics
dotplot <-
    function(group, y, jiggle, rotate=FALSE, ...)
{
    stopifnot(length(y) == length(group))
    if(length(unique(y)) < length(unique(group)))
        warning('Seems like maybe "group" and "y" got switched.')

    # horizontal jiggling
    if(missing(jiggle) || is.null(jiggle))
        jiggle <- broman::jiggle(group, y, "fixed")
    else if(is.character(jiggle))
        jiggle <- broman::jiggle(group, y, jiggle)
    else # otherwise, numeric vector
        stopifnot(length(jiggle) == length(y))

    # turn group into numbers 1, 2, ..., n_group
    if(is.factor(group)) {
        ugroup <- levels(group)
        group <- as.numeric(group)
    }
    else {
        ugroup <- sort(unique(group))
        group <- match(group, ugroup)
    }
    n_group <- length(ugroup)

    # this is to deal with varying inputs
    hidedotplot <-
        function(group, y, rotate=FALSE,
                 vlines=NULL, vlines.col="white", vlines.lwd=1,
                 hlines=NULL, hlines.col="white", hlines.lwd=1,
                 xat=NULL, xlim=NULL, xaxs="r", xlab=NULL,
                 yat=NULL, ylim=NULL, yaxs="r", ylab=NULL,
                 las=1, pch=21, bg="slateblue", ...)

        {
            if(!rotate) {
                xlim <- c(0.5, n_group+0.5)
                vlines <- 1:n_group
                vlines.col <- "gray70"
                vlines.lwd <- 4
                xat <- NA
                if(is.null(xlab)) xlab <- ""

                # deal with vlines/lines ** FIX ME **

                grayplot(group+jiggle, y,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab, las=las,
                         pch=pch, bg=bg, ...)
                axis(side=1, at=vlines, ugroup, las=las)

            }
            else {
                ylim <- c(0.5, n_group+0.5)
                hlines <- 1:n_group
                hlines.col <- "gray70"
                hlines.lwd <- 4
                yat <- NA
                if(is.null(ylab)) ylab <- "Group"

                grayplot(y, group + jiggle,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab,
                         v_over_h=TRUE, las=las, pch=pch, bg=bg, ...)
                axis(side=2, at=hlines, ugroup, las=las)
            }

        }

    hidedotplot(group, y, rotate, ...)
    invisible()
}
