#' maximum of absolute value
#'
#' Take the maximum of the absolute values of the input
#'
#' @param x a numeric vector or array
#' @param na.rm a logical indicating whether missing values should be removed.
#'
#' @return The maximum of the absolute value of the input
#'
#' @examples
#' x <- c(5, -2, 8, -20, 2.3)
#' maxabs(x)
#' @export
maxabs <-
    function(x, na.rm=FALSE)
    max(abs(x), na.rm=na.rm)
