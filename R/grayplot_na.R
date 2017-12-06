#  grayplot_na
#'
#' Scatterplot with missing values indicated
#'
#' Scatterplot with a gray background and with points with missing
#' values shown in separate panels near the margins.
#'
#' @md
#'
#' @param x Coordinates of points in the plot
#'
#' @param y Coordinates of points in the plot (optional)
#'
#' @param type Plot type (points, lines, etc.)
#'
#' @param bgcolor Background color
#'
#' @param v_over_h If `TRUE`, place vertical grid lines on top of
#' the horizontal ones.
#'
#' @param pch point type
#' @param bg Background color in points
#' @param col Color of outer circle in points
#'
#' @param force Indicates whether to force the NA box (on the x-axis,
#' y-axis, or both) even when there are no missing values.
#'
#' @param ... Optional graphics arguments
#'
#' @details
#' Calls [graphics::plot()] with `type="n", then
#' [graphics::rect()] to get the background, and then
#' [graphics::points()].
#'
#' There are a bunch of hidden graphical arguments you can include:
#' `na.width` controls the proportional width devoted to the NA boxes,
#' and `na.gap` the proportion for the gap between the NA boxes and
#' the main plot region. `mgp.x` and `mgp.y` (like `mgp`, for
#' controlling parameters of axis labels, but separate for x- and
#' y-axis). Also `hlines` to indicate locations of of horizontal
#' gridlines, and `hlines.col`, `hlines.lwd`, and `hlines.lty` to set
#' their color, width, and type. `hlines=NA` suppresses the grid
#' lines. Similarly `vlines`, `vlines.col`, `vlines.lwd`, and
#' `vlines.lty`. `xat` and `yat` are for specifying the locations of
#' x- and y-axis labels, respectively. `xat=NA` and `yat=NA` indicate
#' no labels.
#'
#'
#' @export
#' @importFrom graphics plot title rect axis abline points
#'
#' @return
#' None.
#'
#' @examples
#' \dontshow{set.seed(97536917)}
#' n <- 100
#' x <- rnorm(n)
#' y <- x+rnorm(n, 0, 0.7)
#' x[sample(n, 10)] <- NA
#'
#' grayplot_na(x, y)
#'
#' grayplot_na(x, y, force="y")
#'
#' y[sample(n, 10)] <- NA
#' grayplot_na(x, y)
#'
#' @seealso [grayplot()], [dotplot()]
#'
#' @keywords
#' graphics
grayplot_na <-
    function(x, y=NULL, type="p", bgcolor="gray90", v_over_h=FALSE,
             pch=21, bg="lightblue", col="black",
             force=c("none", "x", "y", "both"), ...)
{
    if(missing(x) || is.null(x)) stop("x unspecified")
    force <- match.arg(force)

    # this is to deal with varying inputs (did "..." include xaxt or not?)
    hidegrayplot_na <-
        function(x, y, ..., type="p",
                 hlines=NULL, hlines.col="white", hlines.lty=1, hlines.lwd=1,
                 vlines=NULL, vlines.col="white", vlines.lty=1, vlines.lwd=1,
                 xat=NULL, yat=NULL, bgcolor="gray90", xaxt="n", yaxt="n",
                 col.lab=par("col.lab"),
                 xlim=NULL, ylim=NULL,
                 xlab=NULL, ylab=NULL, xname, yname,
                 xaxs="i", yaxs="i",
                 pch=21, bg="lightblue", col="black",
                 las=1, mgp.x=c(2.6, 0.5, 0), mgp.y=c(2.6, 0.5, 0),
                 force=c("none", "x", "y", "both"),
                 v_over_h=FALSE, na.width=0.06, na.gap=0.01,
                 main="")
        {
            force <- match.arg(force)
            dots <- list(...)

            if(na.width >= 1 || na.width <= 0)
                stop("na.width must be between 0 and 1")
            if(na.gap >= na.width || na.gap <= 0)
                stop("na.gap must be between 0 and na.width")

            if("mgp" %in% names(dots) && missing(mgp.x))
                mgp.x <- dots$mgp
            if("mgp" %in% names(dots) && missing(mgp.y))
                mgp.y <- dots$mgp

            if(is.null(y)) {
                if(is.null(xlab)) xlab <- "Index"
                if(is.null(ylab)) ylab <- xname
                y <- x
                x <- seq(along=x)
            }
            else {
                if(is.null(xlab)) xlab <- xname
                if(is.null(ylab)) ylab <- yname
            }

            if(is.null(ylim)) {
                ylim <- range(y, na.rm=TRUE)
                ylim[1] <- ylim[1]-diff(ylim)*0.02
                ylim[2] <- ylim[2]+diff(ylim)*0.02
            }
            if(is.null(hlines)) {
                if(!is.null(yat))
                    hlines <- yat
                else
                    hlines <- pretty(ylim)
            }
            else if(length(hlines)==1 && is.na(hlines))
                hlines <- NULL

            if(is.null(xlim)) {
                xlim <- range(x, na.rm=TRUE)
                xlim[1] <- xlim[1]-diff(xlim)*0.02
                xlim[2] <- xlim[2]+diff(xlim)*0.02
            }
            if(is.null(vlines)) {
                if(!is.null(xat))
                    vlines <- xat
                else
                    vlines <- pretty(xlim)
            }
            else if(length(vlines)==1 && is.na(vlines))
                vlines <- NULL
            if(is.null(xat)) {
                xat <- pretty(xlim)
                xat <- xat[xat >= xlim[1] & xat <= xlim[2]]
            }
            if(is.null(yat)) {
                yat <- pretty(ylim)
                yat <- yat[yat >= ylim[1] & yat <= ylim[2]]
            }
            if(!is.null(vlines)) vlines <- vlines[vlines >= xlim[1] & vlines <= xlim[2]]
            if(!is.null(hlines)) hlines <- hlines[hlines >= ylim[1] & hlines <= ylim[2]]


            pin <- par("pin")
            xna.width <- pin[1]*na.width
            yna.width <- pin[2]*na.width
            width <- max(c(xna.width, yna.width))
            xna.width <- width/pin[1]
            yna.width <- width/pin[2]
            xna.gap <- pin[1]*na.gap
            yna.gap <- pin[2]*na.gap
            gap <- max(c(xna.gap, yna.gap))
            xna.gap <- gap/pin[1]
            yna.gap <- gap/pin[2]

            # whether to include the x and y NA boxes
            x_na <- y_na <- FALSE
            if(any(is.na(x)) || force %in% c("x", "both")) x_na <- TRUE
            if(any(is.na(y)) || force %in% c("y", "both")) y_na <- TRUE

            if(x_na) {
                xlim_expand <- c(xlim[2]-diff(xlim)/(1-xna.width), xlim[2])
                xlim_na <- c(xlim_expand[1], xlim_expand[1]+(xlim[1]-xlim_expand[1])*(1-xna.gap/xna.width))
                if(!is.null(vlines)) vlines <- c(vlines, mean(xlim_na))
            } else {
                xlim_expand <- xlim
            }
            if(y_na) {
                ylim_expand <- c(ylim[2]-diff(ylim)/(1-yna.width), ylim[2])
                ylim_na <- c(ylim_expand[1], ylim_expand[1]+(ylim[1]-ylim_expand[1])*(1-yna.gap/yna.width))
                if(!is.null(hlines)) hlines <- c(hlines, mean(ylim_na))
            } else {
                ylim_expand <- ylim
            }

            plot(x, y, type="n", xaxt="n", yaxt="n", xlab="", ylab="",
                 xlim=xlim_expand, ylim=ylim_expand, xaxs="i", yaxs="i", bty="n",
                 main=main)
            rect(xlim[1], ylim[1], xlim[2], ylim[2], col=bgcolor, border="black")
            if(x_na) rect(xlim_na[1], ylim[1], xlim_na[2], ylim[2],
                          col=bgcolor, border="black", xpd=TRUE)
            if(y_na) rect(xlim[1], ylim_na[1], xlim[2], ylim_na[2],
                          col=bgcolor, border="black", xpd=TRUE)
            if(x_na && y_na) rect(xlim_na[1], ylim_na[1], xlim_na[2], ylim_na[2],
                                  col=bgcolor, border="black", xpd=TRUE)

            # axis titles
            title(xlab=xlab, mgp=mgp.x, col.lab=col.lab)
            title(ylab=ylab, mgp=mgp.y, col.lab=col.lab)

            # x axis: if adding white lines, skip the tick marks and move the numbers closer
            if(!(length(xat)==1 && is.na(xat))) { # if a single NA, skip x-axis
                if(!is.null(vlines)) {
                    axis(side=1, at=xat, mgp=mgp.x, tick=FALSE, las=las)
                    if(x_na) axis(side=1, at=mean(xlim_na), "NA", mgp=mgp.x, tick=FALSE, las=las)
                } else {
                    axis(side=1, at=xat, las=las)
                    if(x_na) axis(side=1, at=mean(xlim_na), "NA", las=las)
                }
            }

            # y axis: like the x-axis
            if(!(length(yat)==1 && is.na(yat))) { # if a single NA, skip y-axis
                if(!is.null(hlines)) {
                    axis(side=2, at=yat, mgp=mgp.y, tick=FALSE, las=las)
                    if(y_na) axis(side=2, at=mean(ylim_na), "NA", mgp=mgp.y, tick=FALSE, las=las)
                } else {
                    axis(side=2, at=yat, las=las)
                    if(y_na) axis(side=2, at=mean(ylim_na), "NA", las=las)
                }
            }

            if(!is.null(vlines) && !v_over_h)
                abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)
            if(!is.null(hlines))
                abline(h=hlines, col=hlines.col, lty=hlines.lty, lwd=hlines.lwd)
            if(!is.null(vlines) && v_over_h)
                abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)

            points(x, y, pch=pch, bg=bg, col=col, type=type, ...)
            if(x_na) {
                n_na <- sum(is.na(x) & !is.na(y))
                if(n_na > 0) {
                    xnapos <- runif(n_na, xlim_na[1]+diff(xlim_na)*0.2, xlim_na[2]-diff(xlim_na)*0.2)
                    points(xnapos, y[is.na(x) & !is.na(y)],
                           pch=pch, bg=bg, col=col, ...)
                }
            }
            if(y_na) {
                n_na <- sum(is.na(y) & !is.na(x))
                if(n_na > 0) {
                    ynapos <- runif(n_na, ylim_na[1]+diff(ylim_na)*0.2, ylim_na[2]-diff(ylim_na)*0.2)
                    points(x[!is.na(x) & is.na(y)], ynapos,
                           pch=pch, bg=bg, col=col, ...)
                }
            }
            if(x_na & y_na) {
                n_na <- sum(is.na(y) & is.na(x))
                if(n_na > 0) {
                    xnapos <- runif(n_na, xlim_na[1]+diff(xlim_na)*0.2, xlim_na[2]-diff(xlim_na)*0.2)
                    ynapos <- runif(n_na, ylim_na[1]+diff(ylim_na)*0.2, ylim_na[2]-diff(ylim_na)*0.2)
                    points(xnapos, ynapos, pch=pch, bg=bg, col=col, ...)
                }

            }

            # add black borders
            rect(xlim[1], ylim[1], xlim[2], ylim[2], col=NULL, border="black")
            if(x_na) rect(xlim_na[1], ylim[1], xlim_na[2], ylim[2],
                          col=NULL, border="black", xpd=TRUE)
            if(y_na) rect(xlim[1], ylim_na[1], xlim[2], ylim_na[2],
                          col=NULL, border="black", xpd=TRUE)
            if(x_na && y_na) rect(xlim_na[1], ylim_na[1], xlim_na[2], ylim_na[2],
                                  col=NULL, border="black", xpd=TRUE)
        }

    hidegrayplot_na(x=x, y=y, type=type, bgcolor=bgcolor,
                    xname=substitute(x), yname=substitute(y),
                    pch=pch, bg=bg, col=col,
                    v_over_h=v_over_h, force=force, ...)
    invisible()
}
