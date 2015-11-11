# Utility function
#     returns binary representation of 1:(2^n)
binary.v <-
    function(n)
{
    x <- 1:(2^n)
    mx <- max(x)
    digits <- floor(log2(mx))
    ans <- 0:(digits-1); lx <- length(x)
    x <- matrix(rep(x,rep(digits, lx)),ncol=lx)
    (x %/% 2^ans) %% 2
}

# Function to perform a paired permutation test
#     Input: differences, d
#            no. permutations
# Use paired.perm.test(d, n.perm=NULL) to
#     do the exact test
#  paired.perm.test
#'
#' Paired permutation t-test
#'
#' Calculates a p-value for a paired t-test via permutations.
#'
#' @param d A numeric vector (of differences).
#'
#' @param n.perm Number of permutations to perform.  If NULL, all
#'     possible permutations are considered, and an exact p-value is
#'     calculated.
#'
#' @param pval If TRUE, return just the p-value.  If FALSE, return the
#'  actual permutation results (with the observed statistic as an
#'  attribute, \code{"tobs"}).
#'
#' @details
#' This calls the function \code{\link[stats]{t.test}} to calculate a
#'   t-statistic comparing the mean of \code{d} to 0.  Permutations
#'   are perfomed to give an exact or approximate conditional p-value.
#'
#' @export
#' @return
#' If \code{pval=TRUE}, the output is a single number: the P-value
#'   testing for the symmetry about 0 of the distribution of the population
#'   from which \code{d} was drawn.
#'   If \code{pval=FALSE}, the output is a vector of the t statistics from
#'   the permutations.  An attributed \code{"tobs"} contains the t
#'   statistic with the observed data.
#'
#' @examples
#' x <- c(43.3, 57.1, 35.0, 50.0, 38.2, 31.2)
#' y <- c(51.9, 95.1, 90.0, 49.7, 101.5, 74.1)
#' paired.perm.test(x-y)
#'
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link{perm.test}}
#'
#' @keywords
#' htest
paired.perm.test <-
    function(d, n.perm=NULL, pval=TRUE)
{
    n <- length(d)
    tobs <- t.test(d)$statistic
    if(is.null(n.perm)) { # do exact test
        ind <- binary.v(n)
        allt <- apply(ind,2,function(x,y)
                      t.test((2*x-1)*y)$statistic,d)
    }
    else { # do n.perm samples
        allt <- 1:n.perm
        for(i in 1:n.perm)
            allt[i] <- t.test(d*sample(c(-1,1),n,replace=TRUE))$statistic
    }
    if(pval) return(mean(abs(allt) >= abs(tobs)))
    attr(allt, "tobs") <- tobs
    allt
}


# Function to perform permutation test
#     x, y = the two samples
#     n.perm = number of permutations
#     var.equal = passed to the function t.test
# Use perm.test(x, y, n.perm=NULL)
#     to get exact P-value
#  perm.test
#'
#' Permutation t-test
#'
#' Calculates a p-value for a t-test via permutations.
#'
#' @param x A numeric vector.
#'
#' @param y A second numeric vector.
#'
#' @param n.perm Number of permutations to perform.  If NULL, all
#'     possible permutations are considered, and an exact p-value is
#'     calculated.
#'
#' @param var.equal A logical variable indicating whether to treat the two
#'     population variances as being equal.
#'
#' @param pval If TRUE, return just the p-value.  If FALSE, return the
#'  actual permutation results (with the observed statistic as an
#'  attribute, \code{"tobs"}).
#'
#' @details
#' This calls the function \code{\link[stats]{t.test}} to calculate a
#'   t-statistic comparing the vectors \code{x} and \code{y}.  Permutations
#'   are perfomed to give an exact or approximate conditional p-value.
#'
#' @export
#' @importFrom stats t.test
#'
#' @return
#' If \code{pval=TRUE}, the output is a single number: the P-value
#'   testing for a difference in the distributions of the populations from
#'   which \code{x} and \code{y} were drawn.
#'   If \code{pval=FALSE}, the output is a vector of the t statistics from
#'   the permutations.  An attributed \code{"tobs"} contains the t
#'   statistic with the observed data.
#'
#' @examples
#' x <- c(43.3, 57.1, 35.0, 50.0, 38.2, 61.2)
#' y <- c(51.9, 95.1, 90.0, 49.7, 101.5, 74.1)
#' perm.test(x,y)
#'
#' @seealso
#' \code{\link[stats]{t.test}}, \code{\link{paired.perm.test}}
#'
#' @keywords
#' htest
perm.test <-
    function(x, y, n.perm=NULL, var.equal=TRUE, pval=TRUE)
{
    # number of data points
    kx <- length(x)
    ky <- length(y)
    n <- kx + ky

    # Data re-compiled
    X <- c(x,y)
    z <- rep(1:0,c(kx,ky))

    tobs <- t.test(x,y,var.equal=var.equal)$statistic

    if(is.null(n.perm)) { # do exact permutation test
        o <- binary.v(n)  # indicator of all possible samples
        o <- o[,apply(o,2,sum)==kx]
        nc <- choose(n,kx)
        allt <- 1:nc
        for(i in 1:nc) {
            xn <- X[o[,i]==1]
            yn <- X[o[,i]==0]
            allt[i] <- t.test(xn,yn,var.equal=var.equal)$statistic
        }
    }
    else { # do 1000 permutations of the data
        allt <- 1:n.perm
        for(i in 1:n.perm) {
            z <- sample(z)
            xn <- X[z==1]
            yn <- X[z==0]
            allt[i] <- t.test(xn,yn,var.equal=var.equal)$statistic
        }
    }
    if(pval) return(mean( abs(allt) >= abs(tobs) ))
    attr(allt, "tobs") <- tobs
    allt
}
