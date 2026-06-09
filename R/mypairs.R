######################################################################
# mypairs: Like the pairs() function, but doing just the upper
#          triangle and taking par() arguments in the "..."
######################################################################
#'
#' My scatterplot matrix
#'
#' A matrix of scatterplots is produced; it's similar to
#'   [graphics::pairs()], but with only the upper triangle,
#'   and call [grayplot()].
#'
#' @param x A numeric matrix or data frame.
#'
#' @param show_na If TRUE, use [grayplot_na()] for the plots rather than [grayplot()]
#'
#' @param ... Passed to the [grayplot()] function.
#'
#' @details
#' This is like the function [graphics::pairs()], but
#'   only the upper triangle is produced.
#'
#' @export
#' @return
#' None.
#'
#' @examples
#' v <- rbind(c(1,0.5,0.2),c(0.5,1,0.9),c(0.2,0.9,1))
#' x <- rmvn(500, rep(5,3), v)
#' colnames(x) <- c("V1", "V2", "V3")
#' group_col <- sample(crayons(c("Blue","Red")), 500, repl=TRUE)
#' mypairs(x, pch=21, bg=group_col)
#'
#' # add some missing data to the first two columns
#' for(i in 1:2) x[sample(500, 20),i] <- NA
#' mypairs(x, pch=21, bg=group_col)
#' mypairs(x, pch=21, bg=group_col, show_na=FALSE)
#' mypairs(x, pch=21, bg=group_col, force="both")
#'
#' @seealso [graphics::pairs()], [grayplot()]
#'
#' @keywords
#' hplot
mypairs <-
    function(x, show_na=TRUE, ...)
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
            if(show_na) grayplot_na(x[,j], x[,i], xlab=nam[j], ylab=nam[i], ...)
            else grayplot(x[,j], x[,i], xlab=nam[j], ylab=nam[i], ...)
        }
    }
}
