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
#'    If a single number, we take it to be the correlation between all pairs,
#'    in which case the variances are taken to be 1.
#'
#' @details
#' Uses the Cholesky decomposition of the matrix `V`, obtained by
#'   [base::chol()].
#'
#' @importFrom stats rnorm
#' @export
#' @return
#' A matrix of size n x `length(mu)`.  Each row corresponds to a
#'   separate replicate.
#'
#' @examples
#' x <- rmvn(100, c(1,2),matrix(c(1,1,1,4),ncol=2))
#'
#' @seealso
#' [stats::rnorm()]
#'
#' @keywords
#' datagen
rmvn <-
    function(n, mu=0, V=matrix(1))
{
    p <- length(mu)

    if(is.numeric(V) && length(V)==1) {
        # if single number, take it to be the correlation
        V <- diag(rep(1-V, p)) + V
    }

    if(any(is.na(match(dim(V),p))))
        stop("V should be ", p, "x", p)

    D <- chol(V)

    matrix(rnorm(n*p),ncol=p) %*% D + rep(mu,rep(n,p))
}
