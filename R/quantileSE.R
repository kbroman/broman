######################################################################
# quantileSE: calculate quantiles plus approximate SEs
######################################################################
#'
#' Sample quantiles and their standard errors
#'
#' Calculate sample quantiles and their estimated standard errors.
#'
#' @param x Numeric vector whose sample quantiles are wanted.
#'
#' @param p Numeric vector with values in [0,1].
#'
#' @param bw Bandwidth to use in the density estimation.
#'
#' @param na.rm Logical; if true, and \code{NA} and \code{NaN}'s are
#'   removed from \code{x} before the quantiles are computed.
#'
#' @param names Logical; if true, the column names of the result is set to
#' the values in \code{p}.
#'
#' @details
#' The sample quantiles are calculated with the function
#'   \code{\link[stats]{quantile}}.
#'   Standard errors are obtained by the asymptotic approximation described
#'   in Cox and Hinkley (1974).  Density values are estimated using a
#'   kernel density estimate with the function \code{\link[stats]{density}}.
#'
#' @export
#' @importFrom stats density quantile
#'
#' @return
#' A matrix of size 2 x \code{length(p)}.  The first row contains the
#'   estimated quantiles; the second row contains the corresponding
#'   estimated standard errors.
#'
#' @examples
#' quantileSE(rchisq(1000,4), c(0.9,0.95))
#'
#' @seealso
#' \code{\link[stats]{quantile}}, \code{\link[stats]{density}}
#'
#' @keywords
#' univar
quantileSE <-
    function(x, p=0.95, bw, na.rm=TRUE, names=TRUE)
{
    if(na.rm) x <- x[!is.na(x)]
    quant <- quantile(x,p)
    R <- sqrt(p*(1-p)/length(x))
    if(missing(bw))
        f <- sapply(quant, function(a,b) density(b,from=a,to=a,n=1)$y,x)
    else
        f <- sapply(quant, function(a,b) density(b,bw=bw,from=a,to=a,n=1)$y,x)

    out <- rbind(quantile=quant,SE=R/f)
    if(names) colnames(out) <- as.character(p)
    out
}
