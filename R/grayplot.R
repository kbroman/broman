#  grayplot
#'
#' Plot with a gray background
#'
#' Like the plot function, but using a gray background just
#'   for the plot regin.
#'
#' @param x Coordinates of points in the plot
#'
#' @param y Coordinates of points in the plot (optional)
#'
#' @param ... Optional graphics arguments
#'
#' @param type Plot type (points, lines, etc.)
#'
#' @param hlines Locations of horizontal grid lines; use \code{hlines=NA} to prevent horizontal grid lines
#'
#' @param hlines.col Colors of horizontal grid lines
#'
#' @param hlines.lty Line type of horizontal grid lines
#'
#' @param hlines.lwd Line width of horizontal grid lines
#'
#' @param vlines Locations of vertical grid lines; use \code{vlines=NA} to prevent vertical grid lines
#'
#' @param vlines.col Colors of vertical grid lines
#'
#' @param vlines.lty Line type of vertical grid lines
#'
#' @param vlines.lwd Line width of vertical grid lines
#'
#' @param xat Locations for x-axis labels; \code{xat=NA} indicates no labels
#'
#' @param yat Locations for y-axis labels; \code{yat=NA} indicates no labels
#'
#' @param bgcolor Background color
#'
#' @param v_over_h If \code{TRUE}, place vertical grid lines on top of
#' the horizontal ones.
#'
#' @details
#' Calls \code{\link[graphics]{plot}} with \code{type="n"}, then
#'   \code{\link[graphics]{rect}} to get the background, and then
#'   \code{\link[graphics]{points}}.
#'   Additional arguments you can include: \code{mgp.x} and \code{mgp.y}
#'   (like \code{mgp}, for controlling parameters of axis labels, but
#'   separate for x- and y-axis).
#'
#' @export
#' @importFrom graphics plot title rect axis abline points
#'
#' @return
#' None.
#'
#' @examples
#' \dontshow{set.seed(97536917)}
#' x <- rnorm(100)
#' y <- x+rnorm(100, 0, 0.7)
#' grayplot(x, y, col="blue", pch=16)
#' at <- seq(-3, 3)
#' grayplot(x, y, col="blue", pch=16, hlines=at, vlines=at)
#' grayplot(x, col="violet", pch=16, bgcolor="gray90",
#'          hlines=seq(-4, 4, by=0.5), hlines.lwd=c(3,1),
#'          vlines=seq(0, 100, by=5), vlines.lwd=c(3,1,1,1))
#'
#' @seealso
#' \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{par}},
#'   \code{\link[graphics]{rect}},
#'   \code{\link[graphics]{points}}
#'
#' @keywords
#' graphics
grayplot <-
    function(x, y, ..., type="p", hlines, hlines.col="white", hlines.lty=1, hlines.lwd=1,
             vlines, vlines.col="white", vlines.lty=1, vlines.lwd=1,
             xat, yat, bgcolor="gray80", v_over_h=FALSE)
{
    if(missing(x) || is.null(x)) stop("x unspecified")
    if(missing(y)) y <- NULL
    if(missing(xat)) xat <- NULL
    if(missing(yat)) yat <- NULL
    if(missing(hlines)) hlines <- NULL
    if(missing(vlines)) vlines <- NULL

    # this is to deal with varying inputs (did "..." include xaxt or not?)
    hidegrayplot <-
        function(x, y, ..., type="p", hlines, hlines.col, hlines.lty, hlines.lwd,
                 vlines, vlines.col, vlines.lty, vlines.lwd,
                 xat=pretty(x), yat=pretty(y), bgcolor="gray80", xaxt="n", yaxt="n",
                 col.lab=par("col.lab"),
                 xlim, ylim,
                 xlab, ylab, xname, yname,
                 las=1, mgp.x=c(2.6, 0.5, 0), mgp.y=c(2.6, 0.5, 0),
                 v_over_h=FALSE)
        {
            dots <- list(...)
            if("mgp" %in% names(dots) && missing(mgp.x))
                mgp.x <- dots$mgp
            if("mgp" %in% names(dots) && missing(mgp.y))
                mgp.y <- dots$mgp

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

            if(missing(ylim) || is.null(ylim))
                ylim <- range(y, na.rm=TRUE)
            if(missing(hlines) || is.null(hlines)) {
                if(!missing(yat) && !is.null(yat))
                    hlines <- yat
                else
                    hlines <- pretty(ylim)
            }
            else if(length(hlines)==1 && is.na(hlines))
                hlines <- NULL

            if(missing(xlim) || is.null(xlim))
                xlim <- range(x, na.rm=TRUE)
            if(missing(vlines) || is.null(vlines)) {
                if(!missing(xat) && !is.null(xat))
                    vlines <- xat
                else
                    vlines <- pretty(xlim)
            }
            else if(length(vlines)==1 && is.na(vlines))
                vlines <- NULL

            # blank plot
            if(is.null(y))
                plot(seq(along=x), x, ..., type="n", xaxt="n", yaxt="n", xlab="", ylab="",
                     xlim=xlim, ylim=ylim)
            else
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

            if(is.null(y)) points(seq(along=x), x, ..., type=type)
            else points(x, y, ..., type=type)

            # add black border again
            abline(v=u[1:2], h=u[3:4])
        }

    hidegrayplot(x=x, y=y, ..., type=type, hlines=hlines, hlines.col=hlines.col,
                 hlines.lty=hlines.lty, hlines.lwd=hlines.lwd,
                 vlines=vlines, vlines.col=vlines.col,
                 vlines.lty=vlines.lty, vlines.lwd=vlines.lwd,
                 xat=xat, yat=yat, bgcolor=bgcolor,
                 xname=substitute(x), yname=substitute(y),
                 v_over_h=v_over_h)
    invisible()
}
