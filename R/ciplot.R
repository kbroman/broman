#  ciplot
#'
#' Effect plot with multiple CIs for different groups
#'
#' Uses [grayplot()] to plot a set of confidence intervals.
#'
#' @param est Vector of estimates
#' @param se  Vector of standard errors
#' @param lo  Vector of lower values for the intervals
#' @param hi  Vector of upper values for the intervals
#' @param SEmult SE multiplier to create intervals
#'
#' @param rotate If TRUE, have group as y-axis; default (FALSE) has
#' group on x-axis.
#'
#' @param ... Optional graphics arguments
#'
#' @details Calls [grayplot()] with special choices of
#' graphics parameters, as in [dotplot()].
#'
#' Provide either `se` or both `lo` and `hi`. In the case that `se` is
#' used, the intervals will be `est` +/- `SEmult * se`.
#'
#' Group names are taken from the `names(est)`. If missing, we use capital letters.
#'
#' You can control the CI line widths with `ci_lwd` and the color of
#' the CI segments with `ci_col`. You can control the width of the
#' segments at the top and bottom with `ci_endseg`.
#'
#' @export
#'
#' @return
#' None.
#'
#' @examples
#' \dontshow{set.seed(97536917)}
#' x <- rnorm(40, c(1,3))
#' g <- rep(c("A", "B"), 20)
#' me <- tapply(x, g, mean)
#' se <- tapply(x, g, se)
#' ciplot(me, se)
#' ciplot(me, se, SEmult=1)
#'
#' @seealso [grayplot()], [dotplot()]
#'
#' @keywords
#' graphics
ciplot <-
    function(est, se=NULL, lo=NULL, hi=NULL, SEmult=2,
             rotate=FALSE, ...)
{
    if(is.null(se) && is.null(lo) && is.null(hi)) {
        se <- rep(0, length(est))
    }
    if(!is.null(se) && (!is.null(lo) || !is.null(hi))) {
        warning("Provide either se or both lo and hi; se is being used")
    }
    if(!is.null(se)) {
        stopifnot(length(se) == length(est))

        lo <- est - se*SEmult
        hi <- est + se*SEmult
    } else {
        stopifnot(length(lo) == length(est), length(hi) == length(est))
    }

    group <- names(est)
    if(is.null(group)) {
        group <- rep(LETTERS, length(est))[seq_along(est)]
    }

    ugroup <- group
    group <- seq_along(group)
    n_group <- length(ugroup)

    # this is to deal with varying inputs
    hide_ciplot <-
        function(group, est, lo, hi, rotate=FALSE,
                 vlines=NULL, vlines.col="white", vlines.lwd=1,
                 hlines=NULL, hlines.col="white", hlines.lwd=1,
                 xat=NULL, xlim=NULL, xaxs="r", xlab=NULL,
                 yat=NULL, ylim=NULL, yaxs="r", ylab=NULL,
                 las=1, pch=21, bg="slateblue", ci_col="black",
                 ci_lwd=2, ci_endseg=0.05, ...)

        {
            if(!rotate) {
                xlim <- c(0.5, n_group+0.5)
                vlines <- 1:n_group
                vlines.col <- "gray70"
                vlines.lwd <- 4
                xat <- NA
                if(is.null(xlab)) xlab <- "Group"

                if(is.null(ylim)) ylim <- range(c(lo, hi, est))

                # deal with vlines/lines ** FIX ME **

                grayplot(group, est,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab, las=las, type="n")
                axis(side=1, at=vlines, ugroup, las=las, tick=FALSE, mgp=c(0,0.2,0))

                segments(group, lo, group, hi, col=ci_col, lwd=ci_lwd)
                segments(group-ci_endseg, lo, group+ci_endseg, lo, col=ci_col, lwd=ci_lwd)
                segments(group-ci_endseg, hi, group+ci_endseg, hi, col=ci_col, lwd=ci_lwd)

                points(group, est, pch=pch, bg=bg, ...)

            }
            else {
                ylim <- c(0.5, n_group+0.5)
                hlines <- 1:n_group
                hlines.col <- "gray70"
                hlines.lwd <- 4
                yat <- NA
                if(is.null(ylab)) ylab <- "Group"

                if(is.null(xlim)) xlim <- range(c(lo, hi, est))

                grayplot(est, group,
                         vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                         hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                         xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                         yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab,
                         v_over_h=TRUE, las=las, type="n", ...)
                axis(side=2, at=hlines, ugroup, las=las, tick=FALSE, mgp=c(0,0.3,0))

                segments(lo, group, hi, group, col=ci_col, lwd=ci_lwd)
                segments(lo, group-ci_endseg, lo, group+ci_endseg, col=ci_col, lwd=ci_lwd)
                segments(hi, group-ci_endseg, hi, group+ci_endseg, col=ci_col, lwd=ci_lwd)

                points(est, group, pch=pch, bg=bg, ...)
            }

        }

    hide_ciplot(group, est, lo, hi, rotate, ...)
    invisible()
}
