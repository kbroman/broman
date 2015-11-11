subtrap <-
    function(f, a, b, n=1, ...)
{
    h <- (b-a)/n
    if(n==1) return( h*mean( f(c(a,b), ...)))
    else return(h*sum( f(seq(a+h,b,by=2*h),...) ))
}

#' @export
trap <-
    function(f, a, b, tol=1e-8, max.step=1000, ...)
{
    i.old <- subtrap(f,a,b,1,...); n <- 2
    for(i in 2:max.step) {
        s <- subtrap(f, a, b, n, ...)
        i.new <- i.old/2 + s
        if(abs(i.new-i.old) < tol) break
        i.old <- i.new
        n <- n*2
    }
    i.new
}

#  simp
#'
#' Numerical integration
#'
#' Perform numerical integration by Simpson's rule or the trapezoidal
#'   rule.
#'
#' @aliases trap
#'
#' @param f The integrand; must be a vectorized function.
#'
#' @param a Lower limit of integration.
#'
#' @param b Upper limit of integration.
#'
#' @param tol Tolerance for choosing the number of grid points.
#'
#' @param max.step Log base 2 of the total number of grid points.
#'
#' @param ... Other arguments passed to the integrand, \code{f}.
#'
#' @details
#' Iterately doubles the number of grid points for the numerical
#'   integral, stopping when the integral decreases by less than
#'   \code{tol}.
#'
#' @export
#' @return
#' The integral of \code{f} from \code{a} to \code{b}.
#'
#' @examples
#' f <- function(x) x*x*(1-x)*sin(x*x)
#' I1 <- trap(f,0,2)
#' I2 <- simp(f,0,2)
#'
#' @seealso
#' \code{\link[stats]{integrate}}
#'
#' @keywords
#' math
simp <-
    function(f, a, b, tol=1e-8, max.step=1000, ...)
{
    i.old <- subtrap(f, a, b, 1, ...)*2/3
    n <- 2; old.s <- 0
    for(i in 2:max.step) {
        s <- subtrap(f, a, b, n, ...)
        i.new <- (i.old/2 + (4*s-old.s)/3)
        if(abs(i.new-i.old) < tol) break
        i.old <- i.new; old.s <- s
        n <- n*2
    }
    i.new
}
