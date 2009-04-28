\name{mypairs}
\alias{mypairs}

\title{My scatterplot matrix}

\description{
  A matrix of scatterplots is produced; it's similar to
  \code{\link[graphics]{pairs}}, but with only the upper triangle is
  made. 
}

\usage{
mypairs(x, \dots)
}

\arguments{
 \item{x}{A numeric matrix or data frame.}
 \item{\dots}{Passed to the \code{\link[graphics]{plot}} function.}
}

\details{
  This is like the function \code{\link[graphics]{pairs}}, but
  only the upper triangle is produced.
}

\value{
  None.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
v <- rbind(c(1,0.5,0.2),c(0.5,1,0.9),c(0.2,0.9,1))
x <- rmvn(500, rep(5,3), v)
mypairs(x, col=sample(c("blue","red"), 500, repl=TRUE))
}

\seealso{ \code{\link[graphics]{pairs}} }

\keyword{hplot}
