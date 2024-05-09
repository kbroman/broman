#  grayplot
#'
#' Scatterplot with a gray background
#'
#' Like the plot function, but using a gray background just
#'   for the plot region.
#'
#' @param x Coordinates of points in the plot
#'
#' @param y Coordinates of points in the plot (optional)
#'
#' @param ... Optional graphics arguments
#'
#' @param type Plot type (points, lines, etc.)
#'
#' @param hlines Locations of horizontal grid lines; use `hlines=NA` to prevent horizontal grid lines
#'
#' @param hlines.col Colors of horizontal grid lines
#'
#' @param hlines.lty Line type of horizontal grid lines
#'
#' @param hlines.lwd Line width of horizontal grid lines
#'
#' @param vlines Locations of vertical grid lines; use `vlines=NA` to prevent vertical grid lines
#'
#' @param vlines.col Colors of vertical grid lines
#'
#' @param vlines.lty Line type of vertical grid lines
#'
#' @param vlines.lwd Line width of vertical grid lines
#'
#' @param xat Locations for x-axis labels; `xat=NA` indicates no labels
#'
#' @param yat Locations for y-axis labels; `yat=NA` indicates no labels
#'
#' @param bgcolor Background color
#'
#' @param pch point type
#' @param bg Background color in points
#' @param col Color of outer circle in points
#'
#' @param v_over_h If `TRUE`, place vertical grid lines on top of
#' the horizontal ones.
#'
#' @details
#' Calls `plot()` with `type="n"`, then [graphics::rect()] to
#' get the background, and then [graphics::points()]. Additional
#' arguments you can include: `mgp.x` and `mgp.y` (like `mgp`, for
#' controlling parameters of axis labels, but separate for x- and
#' y-axis).
#'
#' @export
#' @importFrom graphics title rect axis abline points
#'
#' @return
#' None.
#'
#' @examples
#' \dontshow{set.seed(97536917)}
#' x <- rnorm(100)
#' y <- x+rnorm(100, 0, 0.7)
#' grayplot(x, y, col="slateblue", pch=16)
#' at <- seq(-3, 3)
#' grayplot(x, y, col="violetred", pch=16, hlines=at, vlines=at)
#' grayplot(x, col="Orchid", pch=16, bgcolor="gray80",
#'          hlines=seq(-4, 4, by=0.5), hlines.lwd=c(3,1),
#'          vlines=seq(0, 100, by=5), vlines.lwd=c(3,1,1,1))
#'
#' @seealso
#' [dotplot()], [timeplot()], [graphics::par()], [graphics::rect()], [graphics::points()]
#'
#' @keywords
#' graphics
grayplot <-
    function(x, y=NULL, ..., type="p", hlines=NULL, hlines.col="white", hlines.lty=1, hlines.lwd=1,
             vlines=NULL, vlines.col="white", vlines.lty=1, vlines.lwd=1,
             xat=NULL, yat=NULL, bgcolor="gray90",
             pch=21, bg="lightblue", col="black",
             v_over_h=FALSE)
{
    if(missing(x) || is.null(x)) stop("x unspecified")

    # this is to deal with varying inputs (did "..." include xaxt or not?)
    hidegrayplot <-
        function(x, y, ..., type="p", hlines=NULL, hlines.col, hlines.lty, hlines.lwd,
                 vlines=NULL, vlines.col, vlines.lty, vlines.lwd,
                 xat=pretty(x), yat=pretty(y), bgcolor="gray90", xaxt="n", yaxt="n",
                 col.lab=par("col.lab"),
                 xlim=NULL, ylim=NULL,
                 xlab, ylab, xname, yname,
                 las=1, mgp=c(2.1, 0.5, 0), mgp.x=NULL, mgp.y=NULL,
                 pch=21, bg="lightblue", col="black",
                 v_over_h=FALSE)
        {
            if(is.null(mgp.x)) mgp.x <- mgp
            if(is.null(mgp.y)) mgp.y <- mgp

            if(is.null(y)) {
                if(missing(xlab)) xlab <- "Index"
                if(missing(ylab)) ylab <- xname
                y <- x
                x <- seq(along=x)
            }
            else {
                if(missing(xlab)) xlab <- xname
                if(missing(ylab)) ylab <- yname
            }

            if(is.null(ylim))
                ylim <- range(y, na.rm=TRUE)
            if(is.null(hlines)) {
                if(!is.null(yat))
                    hlines <- yat
                else
                    hlines <- pretty(ylim)
            }
            else if(length(hlines)==1 && is.na(hlines))
                hlines <- NULL

            if(is.null(xlim))
                xlim <- range(x, na.rm=TRUE)
            if(is.null(vlines)) {
                if(!is.null(xat))
                    vlines <- xat
                else
                    vlines <- pretty(xlim)
            }
            else if(length(vlines)==1 && is.na(vlines))
                vlines <- NULL

            # blank plot
            plot(x, y, ..., type="n", xaxt="n", yaxt="n", xlab="", ylab="",
                 xlim=xlim, ylim=ylim)

            # axis titles
            title(xlab=xlab, mgp=mgp.x, col.lab=col.lab)
            title(ylab=ylab, mgp=mgp.y, col.lab=col.lab)

            # add gray rectangle
            u <- par("usr")
            rect(u[1], u[3], u[2], u[4], col=bgcolor, border="black")

            # x axis: if adding white lines, skip the tick marks and move the numbers closer
            if(!(!is.null(xat) && length(xat)==1 && is.na(xat))) { # if a single NA, skip x-axis
                if(!is.null(xat)) {
                    if(!is.null(vlines))
                        axis(side=1, at=xat, mgp=mgp.x, tick=FALSE, las=las)
                    else
                        axis(side=1, at=xat, las=las)
                }
                else {
                    if(!is.null(vlines))
                        axis(side=1, mgp=mgp.x, tick=FALSE, las=las)
                    else
                        axis(side=1, las=las)
                }
            }

            # y axis: like the x-axis
            if(!(!is.null(yat) && length(yat)==1 && is.na(yat))) { # if a single NA, skip y-axis
                if(!is.null(yat)) {
                    if(!is.null(hlines))
                        axis(side=2, at=yat, mgp=mgp.y, tick=FALSE, las=las)
                    else
                        axis(side=2, at=yat, las=las)
                }
                else {
                    if(!is.null(hlines))
                        axis(side=2, mgp=mgp.y, tick=FALSE, las=las)
                    else
                        axis(side=2, las=las)
                }
            }

            if(!is.null(vlines) && !v_over_h)
                abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)
            if(!is.null(hlines))
                abline(h=hlines, col=hlines.col, lty=hlines.lty, lwd=hlines.lwd)
            if(!is.null(vlines) && v_over_h)
                abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)

            points(x, y, ..., pch=pch, bg=bg, col=col, type=type)

            # add black border again
            abline(v=u[1:2], h=u[3:4])
        }

    hidegrayplot(x=x, y=y, ..., type=type, hlines=hlines, hlines.col=hlines.col,
                 hlines.lty=hlines.lty, hlines.lwd=hlines.lwd,
                 vlines=vlines, vlines.col=vlines.col,
                 vlines.lty=vlines.lty, vlines.lwd=vlines.lwd,
                 xat=xat, yat=yat, bgcolor=bgcolor,
                 pch=pch, bg=bg, col=col,
                 xname=substitute(x), yname=substitute(y),
                 v_over_h=v_over_h)
    invisible()
}
