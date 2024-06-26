#  dotplot
#'
#' Dot chart with a gray background
#'
#' Like the [grayplot()] function, but with one axis assumed to be categorical.
#'
#' @param group Categorical coordinates for the plot
#' @param y Coordinates of points in the plot
#'
#' @param rotate If TRUE, have group as y-axis; default (FALSE) has
#' group on x-axis.
#'
#' @param ... Optional graphics arguments
#' @param jiggle Vector of amounts to jiggle the points horizontally,
#' or a character string (`"fixed"` or `"random"`)
#' indicating the jiggling method; see [jiggle()].
#' @param max_jiggle Maximum jiggle value; passed to [jiggle()] as argument `maxvalue`.
#'
#' @details Calls [grayplot()] with special choices of
#' graphics parameters for the case of categorical x.
#'
#' If `group` is a factor, the order of the groups is as in the
#' levels. Otherwise, we take `sort(unique(group))`. So if you want to
#' control the order of the levels, make `group` a factor with the levels
#' in the desired order, for example `group <- factor(group, levels=unique(group))`.
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
#' dotplot(g, x, "fixed")
#' dotplot(g, x, runif(length(g), -0.25, 0.25))
#'
#' @seealso [grayplot()], [timeplot()]
#'
#' @keywords
#' graphics
dotplot <-
    function(group, y, jiggle=NULL, max_jiggle=0.45,
             rotate=FALSE, ...)
{
    stopifnot(length(y) == length(group))
    if(length(unique(y)) < length(unique(group)))
        warning('Seems like maybe "group" and "y" got switched.')

    # omit missing values
    if(any(is.na(group) | is.na(y))) {
        keep <- !is.na(y) & !is.na(group)
        if(sum(keep) == 0)
            stop("All data are missing")

        group <- group[keep]
        if(!is.null(jiggle) && !is.character(jiggle)) {
            stopifnot(length(jiggle) == length(y))
            jiggle <- jiggle[keep]
        }
        y <- y[keep]
    }

    # horizontal jiggling
    if(is.null(jiggle))
        jiggle <- broman::jiggle(group, y, "random", maxvalue=max_jiggle)
    else if(is.character(jiggle))
        jiggle <- broman::jiggle(group, y, jiggle, maxvalue=max_jiggle)
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
                 vlines=NULL, vlines.col=NULL, vlines.lwd=NULL,
                 hlines=NULL, hlines.col=NULL, hlines.lwd=NULL,
                 xat=NULL, xlim=NULL, xaxs="r", xlab=NULL,
                 yat=NULL, ylim=NULL, yaxs="r", ylab=NULL,
                 las=1, pch=21, bg="slateblue", v_over_h=NULL, ...)

        {
            if(!rotate) {
                if(is.null(v_over_h)) v_over_h <- TRUE
                xlim <- c(0.5, n_group+0.5)
                if(is.null(vlines)) {
                    vlines <- 1:n_group
                    xat <- NA
                }
                if(is.null(vlines.col)) vlines.col <- "gray70"
                if(is.null(vlines.lwd)) vlines.lwd <- 4
                if(is.null(hlines.col)) hlines.col <- "white"
                if(is.null(hlines.lwd)) hlines.lwd <- 1
                if(is.null(xlab)) xlab <- "Group"

                grayplot(group+jiggle, y,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab, las=las,
                         pch=pch, bg=bg, ...)
                axis(side=1, at=vlines, ugroup, las=las, tick=FALSE, mgp=c(0,0.2,0))

            }
            else {
                if(is.null(v_over_h)) v_over_h <- FALSE
                ylim <- c(0.5, n_group+0.5)
                if(is.null(hlines)) {
                    hlines <- 1:n_group
                    yat <- NA
                }
                if(is.null(hlines.col)) hlines.col <- "gray70"
                if(is.null(hlines.lwd)) hlines.lwd <- 4
                if(is.null(vlines.col)) vlines.col <- "white"
                if(is.null(vlines.lwd)) vlines.lwd <- 1
                if(is.null(ylab)) ylab <- "Group"

                grayplot(y, group + jiggle,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab,
                         v_over_h=v_over_h, las=las, pch=pch, bg=bg, ...)
                axis(side=2, at=hlines, ugroup, las=las, tick=FALSE, mgp=c(0,0.3,0))
            }

        }

    hidedotplot(group, y, rotate, ...)
    invisible()
}
