\name{runningmean}
\alias{runningmean}

\title{Running mean, sum, or median}

\description{
  Calculates a running mean, sum or median with a specified window.
}

\usage{
runningmean(pos, value, window=1000, what=c("mean", "sum", "median"))
}

\arguments{
 \item{pos}{Positions for the values.}
 \item{value}{Values for which the running mean/sum/median is to be
   applied.}
 \item{window}{Window width.}
 \item{what}{Statistic to use.}
}

\value{
  A vector with the same length as the input, containing the running
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
}

\seealso{ \code{\link{runningratio}} }

\keyword{univar}
