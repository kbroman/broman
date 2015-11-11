#  revrainbow
#'
#' Create vector of colors from blue to red
#'
#' Calls \code{\link[grDevices]{rainbow}} then \code{\link[base]{rev}}
#'
#' @param n Number of colors.
#'
#' @param ... Passed to \code{\link[grDevices]{rainbow}}.
#'
#' @details
#' There's not much to this. It's just \code{rev(rainbow(start=0, end=2/3, ...))}.
#'
#' @export
#' @return
#' Vector of colors, from blue to red.
#'
#' @examples
#' x <- matrix(rnorm(100), ncol=10)
#' image(x, col=revrainbow())
#'
#' @seealso
#' \code{\link[base]{rev}},
#'   \code{\link[grDevices]{rainbow}}
#'
#' @keywords
#' color
revrainbow <-
    function(n=256, ...)
    rev(grDevices::rainbow(start=0, end=2/3, n=n, ...))
