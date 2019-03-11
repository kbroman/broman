#  revgray
#'
#' Create vector of colors from white to black
#'
#' Calls [grDevices::gray()] then [base::rev()]
#'
#' @param n Number of colors.
#'
#' @param ... Passed to [grDevices::gray()].
#'
#' @details
#' There's not much to this. It's just `gray((n:0)/n))`
#'
#' @export
#' @return
#' Vector of colors, from white to black
#'
#' @examples
#' x <- matrix(rnorm(100), ncol=10)
#' image(x, col=revgray())
#'
#' @seealso [grDevices::gray()]
#'
#' @keywords
#' color
revgray <-
    function(n=256, ...)
    grDevices::gray(seq(from=1, to=0, length=n), ...)
