\name{runningmean}
\alias{runningmean}

\title{Running mean, sum, or median}

\description{
  Calculates a running mean, sum or median with a specified window.
}

\usage{
runningmean(pos, value, at, window=1000, what=c("mean", "sum", "median", "sd"))
}

\arguments{
 \item{pos}{Positions for the values.}
 \item{value}{Values for which the running mean/sum/median/sd is to be
   applied.}
 \item{at}{Positions at which running mean (or sum or median or sd) is
 calculated.  If missing, \code{pos} is used.}
 \item{window}{Window width.}
 \item{what}{Statistic to use.}
}

\value{
  A vector with the same length as the input \code{at} (or \code{pos},
  if \code{at} is missing), containing the running
  statistic. 
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
x <- 1:10000
y <- rnorm(length(x))

plot(x,y, xaxs="i", yaxs="i")
lines(x, runningmean(x, y, window=100, what="mean"),
      col="blue", lwd=2)
lines(x, runningmean(x, y, window=100, what="median"),
      col="red", lwd=2)
lines(x, runningmean(x, y, window=100, what="sd"),
      col="green", lwd=2)
}

\seealso{ \code{\link{runningratio}} }

\keyword{univar}
