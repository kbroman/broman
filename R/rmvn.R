#  rmvn
#'
#' Simulate multivariate normal
#'
#' Simulate from a multivariate normal distribution.
#'
#' @param n Number of simulation replicates.
#'
#' @param mu Mean vector.
#'
#' @param V Variance-covariance matrix.
#'
#' @details
#' Uses the Cholesky decomposition of the matrix \code{V}, obtained by
#'   \code{\link[base]{chol}}.
#'
#' @importFrom stats rnorm
#' @export
#' @return
#' A matrix of size n x \code{length(mu)}.  Each row corresponds to a
#'   separate replicate.
#'
#' @examples
#' x <- rmvn(100, c(1,2),matrix(c(1,1,1,4),ncol=2))
#'
#' @seealso
#' \code{\link[stats]{rnorm}}
#'
#' @keywords
#' datagen
rmvn <-
    function(n, mu=0, V=matrix(1))
{
    p <- length(mu)
    if(any(is.na(match(dim(V),p))))
        stop("Dimension problem!")
    D <- chol(V)
    matrix(rnorm(n*p),ncol=p) %*% D + rep(mu,rep(n,p))
}
