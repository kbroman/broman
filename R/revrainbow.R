#  revrainbow
#'
#' Create vector of colors from blue to red
#'
#' Calls [grDevices::rainbow()] then [base::rev()]
#'
#' @param n Number of colors.
#'
#' @param ... Passed to [grDevices::rainbow()].
#'
#' @details
#' There's not much to this. It's just `rev(rainbow(start=0, end=2/3, ...))`.
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
#' [base::rev()],
#'   [grDevices::rainbow()]
#'
#' @keywords
#' color
revrainbow <-
    function(n=256, ...)
    rev(grDevices::rainbow(start=0, end=2/3, n=n, ...))
