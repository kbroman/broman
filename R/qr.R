######################################################################
# qr2: Pulls out Q and R for Q-R decomposition given by qr()
######################################################################
#'
#' The QR decomposition of a matrix
#'
#' Computes the QR decomposition of a matrix.
#'
#' @param x A matrix whose QR decomposition is to be computed.
#'
#' @param tol The tolerance for detecting linear dependencies in the
#'   columns of \code{x}.
#'
#' @details
#' Calls the function \code{\link[base]{qr}} and returns
#'   less compact but more understandable output.
#'
#' @export
#' @return
#' A list of two matrices: Q and R.
#'
#' @examples
#' hilbert <- function(n) { i <- 1:n; 1/outer(i-1,i,"+") }
#' h5 <- hilbert(5);
#' qr2(h5)
#'
#' @seealso
#' \code{\link[base]{qr}}
#'
#' @keywords
#' algebra
qr2 <-
    function(x, tol=1e-7)
{
    qq <- qr(x, tol=tol)
    p <- ncol(x); n <- nrow(x)

    r0 <- matrix(0,p,p)
    r0[row(r0) <= col(r0)] <-
        qq$qr[row(qq$qr) <= col(qq$qr)]
    r0 <- sweep(r0,1,(-1)^(1:p),"*")

    q0 <- qr.qy(qq,diag(1,n)[,1:p])
    q0 <- sweep(q0,2,(-1)^(1:p),"*")

    list(q=q0,r=r0)
}
