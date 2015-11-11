######################################################################
# fisher: "approximate" fisher's exact test
######################################################################
#'
#' Fisher's exact test for a two-way table
#'
#' Performs a sampling version of Fisher's exact test for a two-way
#'   contingency table.
#'
#' @param tab A matrix of counts.
#'
#' @param n.sim Number of samples of permuted tables to consider.
#'
#' @details
#' This is like the function \code{\link[stats]{fisher.test}}, but
#'   calculates an approximate P-value rather than performing a complete
#'   enumeration.  This will be better for large, sparse tables.
#'
#' @importFrom stats chisq.test
#' @export
#' @return
#' A single number: the P-value testing independence of rows and columns
#'   in the table.
#'
#' @examples
#' TeaTasting <- matrix(c(3,1,1,3),nrow=2)
#' fisher(TeaTasting,1000)
#'
#' @seealso
#' \code{\link[stats]{chisq.test}},
#'   \code{\link[stats]{fisher.test}}, \code{\link{chisq}}
#'
#' @keywords
#' htest
fisher <-
    function(tab, n.sim=1000)
{
    bot0 <- sum(lgamma(tab+1))
    bot <- 1:n.sim
    a <- list(rep(row(tab),tab),rep(col(tab),tab))
    for(i in 1:n.sim) {
        a[[1]] <- sample(a[[1]])
        tab2 <- table(a)
        bot[i] <- sum(lgamma(tab2+1))
    }
    mean(bot0 <= bot)
}

######################################################################
# chisq: approximate chi-square test
######################################################################
#'
#' Chi-square test by simuation for a two-way table
#'
#' Calculate a p-value for a chi-square test by Monte Carlo simulation.
#'
#' @param tab A matrix of counts.
#'
#' @param n.sim Number of samples of permuted tables to consider.
#'
#' @details
#' This is like the function \code{\link[stats]{chisq.test}}, but
#'   calculates an approximate P-value rather than refering to
#'   asymptotics.  This will be better for large, sparse tables.
#'
#' @export
#' @return
#' A single number: the P-value testing independence of rows and columns
#'   in the table.
#'
#' @examples
#' TeaTasting <- matrix(c(3,1,1,3),nrow=2)
#' chisq(TeaTasting,1000)
#'
#' @seealso
#' \code{\link[stats]{chisq.test}},
#'   \code{\link[stats]{fisher.test}}, \code{\link{fisher}}
#'
#' @keywords
#' htest
chisq <-
    function(tab, n.sim=1000)
{
    observed <- suppressWarnings(chisq.test(tab)$stat)
    sims <- 1:n.sim
    a <- list(rep(row(tab),tab),rep(col(tab),tab))
    for(i in 1:n.sim) {
        a[[1]] <- sample(a[[1]])
        tab2 <- table(a)
        sims[i] <- suppressWarnings(chisq.test(tab2)$stat)
    }
    mean(sims >= observed)
}
