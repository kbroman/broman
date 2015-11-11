######################################################################
# mypairs: Like the pairs() function, but doing just the upper
#          triangle and taking par() arguments in the "..."
######################################################################
#'
#' My scatterplot matrix
#'
#' A matrix of scatterplots is produced; it's similar to
#'   \code{\link[graphics]{pairs}}, but with only the upper triangle is
#'   made.
#'
#' @param x A numeric matrix or data frame.
#'
#' @param ... Passed to the \code{\link[graphics]{plot}} function.
#'
#' @details
#' This is like the function \code{\link[graphics]{pairs}}, but
#'   only the upper triangle is produced.
#'
#' @export
#' @return
#' None.
#'
#' @examples
#' v <- rbind(c(1,0.5,0.2),c(0.5,1,0.9),c(0.2,0.9,1))
#' x <- rmvn(500, rep(5,3), v)
#' mypairs(x, col=sample(c("blue","red"), 500, repl=TRUE))
#'
#' @seealso
#' \code{\link[graphics]{pairs}}
#'
#' @keywords
#' hplot
mypairs <-
    function(x, ...)
{
    n <- ncol(x)
    if(is.null(colnames(x)))
        nam <- 1:n
    else
        nam <- colnames(x)

    z <- matrix(n^2, n-1, n-1, byrow=TRUE)
    m <- choose(n,2)
    z[!upper.tri(t(z))] <- 1:m
    z <- t(z)
    z[z>m] <- m + 1:sum(z>m)

    graphics::layout(z)

    for(i in 1:(n-1)) {
        for(j in (i+1):n) {
            plot(x[,j], x[,i], xlab=nam[j], ylab=nam[i], ...)
        }
    }
}
