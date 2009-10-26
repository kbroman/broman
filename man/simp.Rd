\name{simp}
\alias{simp}
\alias{trap}

\title{Numerical integration}

\description{
  Perform numerical integration by Simpson's rule or the trapezoidal
  rule. 
}

\usage{
simp(f, a, b, tol=1e-8, max.step=1000, \dots)
trap(f, a, b, tol=1e-8, max.step=1000, \dots)
}

\arguments{
 \item{f}{The integrand; must be a vectorized function.}
 \item{a}{Lower limit of integration.}
 \item{b}{Upper limit of integration.}
 \item{tol}{Tolerance for choosing the number of grid points.}
 \item{max.step}{Log base 2 of the total number of grid points.}
 \item{\dots}{Other arguments passed to the integrand, \code{f}.}
}

\details{
  Iterately doubles the number of grid points for the numerical
  integral, stopping when the integral decreases by less than
  \code{tol}. 
}

\value{
  The integral of \code{f} from \code{a} to \code{b}.
}

\references{
  Press et al. \emph{Numerial recipes in C}.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
f <- function(x) x*x*(1-x)*sin(x*x)
I1 <- trap(f,0,2)
I2 <- simp(f,0,2)
}

\seealso{ \code{\link[stats]{integrate}} }

\keyword{math}
