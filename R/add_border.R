#' Add a rectangular border around a plotting region
#'
#' Add a rectangular border around the region of a figure, to hide any small overlaps.
#'
#' @param border Color for the rectangle's border
#' @param ... Further arguments passed to \code{\link[graphics]{rect}}
#'
#' @export
#'
#' @return Returns the result of \code{par("usr")} - a vector of the
#'     form \code{c(x1, x2, y1, y2)} giving the extremes of the user
#'     coordinates of the plotting region.
#'
#' @export
#'
#' @author
#' Karl W Broman, \email{kbroman@biostat.wisc.edu}
#'
#' @example
#' x <- rnorm(100)
#' y <- 5*x + rnorm(100)
#' grayplot(x,y, pch=21, bg="slateblue")
#' abline(h=0, v=0, col="violetred")
#' add_border()
add_border <-
function(border="black", ...)
{
    u <- par("usr")
    graphics::rect(u[1], u[3], u[2], u[4], border=border, ...)
    invisible(u)
}
