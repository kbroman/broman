######################################################################
# winsorize a vector
#
# (move values above and below the alpha and 1-alpha quantiles
######################################################################
#  winsorize
#'
#' Winsorize a vector
#'
#' For a numeric vector, move values below and above the q and 1-q
#'   quantiles to those quantiles.
#'
#' @param x Numeric vector
#'
#' @param q Lower quantile to use
#'
#' @export
#' @importFrom assertthat assert_that is.number
#'
#' @return
#' A vector like the input \code{x}, but with extreme values moved in to
#'   the \code{q} and \code{1-q} quantiles.
#'
#' @examples
#' x <- sample(c(1:10, rep(NA, 10), 21:30))
#' winsorize(x, 0.2)
#'
#' @keywords
#' utilities
winsorize <-
    function(x, q=0.006)
{
    assert_that(is.numeric(x))
    assert_that(is.number(q), q>=0, q<=1)

    lohi <- stats::quantile(x, c(q, 1-q), na.rm=TRUE)
    if(diff(lohi) < 0) lohi <- rev(lohi)

    x[!is.na(x) & x < lohi[1]] <- lohi[1]
    x[!is.na(x) & x > lohi[2]] <- lohi[2]
    x
}
