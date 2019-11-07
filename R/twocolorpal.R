#  twocolorpal
#'
#' Create vector of colors from blue to white to red
#'
#' Create a two-color palette from one color to another through some third color
#'
#' @param colors Vector of three colors
#'
#' @param n Number of colors in output.
#'
#' @param ... Passed to [grDevices::colorRampPalette()].
#'
#' @return
#' Vector of colors, from blue to white to red
#'
#' @export
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' x <- matrix(rnorm(100, 0.5), ncol=10)
#' mxabs <- max(abs(x))
#' image(x, col=twocolorpal(), zlim=c(-mxabs, mxabs))
#'
#' @seealso [revgray()]
#'
#' @keywords
#' color
twocolorpal <-
    function(colors=c("slateblue", "white", "violetred"), n=256, ...)
{
    stopifnot(length(colors)==3)

    halfn <- ceiling(n/2)

    c(grDevices::colorRampPalette(colors[1:2], ...)(halfn),
      grDevices::colorRampPalette(colors[2:3], ...)(halfn)[-1])
}
