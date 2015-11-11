######################################################################
# normalize:
#  force a matrix of intensities to have columns with the same marginal
#  distributions.
######################################################################
#'
#' Quantile normalization
#'
#' Quantile normalizes two vectors or a matrix.
#'
#' @param x Numeric vector or matrix
#'
#' @param y Optional second numeric vector
#'
#' @details
#' We sort the columns, take averages across rows, and then plug the
#'   averages back into the respective positions.  The marginal
#'   distributions in the columns are thus forced to be the same.
#'   Missing values, which can result in differing numbers of observed
#'   values per column, are dealt with by linear interpolation.
#'
#' @export
#' @return
#' If two vectors, \code{x} and \code{y}, are provided, the output is a
#'   matrix with two columns, with the quantile normalized versions of
#'   \code{x} and \code{y}.
#'   If \code{y} is missing, \code{x} should be a matrix, in which case the
#'   output is a matrix of the same dimensions with the columns quantile
#'   normalized with respect to each other.
#'
#' @examples
#' z <- rmvn(10000, mu=c(0,5,10), V = rbind(c(1,0.5,0.5),c(0.5,1,0.5),c(0.5,0.5,1)))
#' z[sample(prod(dim(z)), 1500)] <- NA
#' pairs(z)
#' br <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length=200)
#' par(mfrow=c(3,1))
#' for(i in 1:3)
#'   hist(z[,i], xlab="z", main=i, breaks=br)
#' zn <- normalize(z)
#' br <- seq(min(zn, na.rm=TRUE), max(zn, na.rm=TRUE), length=200)
#' for(i in 1:3)
#'   hist(zn[,i], xlab="normalized z", main=i, breaks=br)
#' pairs(zn)
#'
#' @keywords
#' utilities
#'
#' @useDynLib broman
normalize <-
    function(x,y)
{
    if(!missing(y)) x <- cbind(x,y)
    if(is.data.frame(x)) x <- as.matrix(x)

    x[abs(x) == Inf] <- NA
    maxval <- max(x, na.rm=TRUE) + 1
    if(any(is.na(x))) x[is.na(x)] <- maxval+100

    n <- nrow(x)
    p <- ncol(x)

    z <- .C("R_normalize",
            as.integer(n),
            as.integer(p),
            x=as.double(x),
            as.double(maxval),
            as.integer(rep((1:n)-1,p)),
            as.double(x),
            PACKAGE="broman")$x

    z[z > maxval] <- NA
    matrix(z, ncol=p)
}
