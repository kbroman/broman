# fac2num
#' Convert a factor to numeric
#'
#' Convert a factor with numeric levels to a non-factor
#'
#' @param x A vector containing a factor with numeric levels
#'
#' @return The input factor made a numeric vector
#'
#' @examples
#' x <- factor(c(3, 4, 9, 4, 9), levels=c(3,4,9))
#' fac2num(x)
#'
#' @export
fac2num <-
    function(x)
{
    nam <- names(x)
    x <- as.numeric(as.character(x))
    names(x) <- nam
    x
}
