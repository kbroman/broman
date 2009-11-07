\name{runningratio}
\alias{runningratio}

\title{Running ratio}

\description{
  Calculates a running ratio; a ratio sum(top)/sum(bottom) in a sliding window.
}

\usage{
runningratio(pos, numerator, denominator, at, window=1000)
}

\arguments{
 \item{pos}{Positions for the values.}
 \item{numerator}{Values for numerator in ratio.}
 \item{denominator}{Values for denominator in ratio.}
 \item{at}{Positions at which running ratio is
 calculated.  If missing, \code{pos} is used.}
 \item{window}{Window width.}
}

\value{
  A vector with the same length as the input \code{at} (or \code{pos},
  if \code{at} is missing), containing the running ratio.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
x <- 1:1000
y <- runif(1000, 1, 5)
z <- runif(1000, 1, 5)

plot(x, runningratio(x, y, z, window=5), type="l", lwd=2)
lines(x, runningratio(x, y, z, window=50), lwd=2, col="blue")
lines(x, runningratio(x, y, z, window=100), lwd=2, col="red")
}

\seealso{ \code{\link{runningmean}} }

\keyword{univar}
