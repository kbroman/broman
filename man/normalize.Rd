\name{normalize}
\alias{normalize}

\title{Quantile normalization}

\description{
  Quantile normalizes two vectors or a matrix.
}

\usage{
normalize(x, y)
}

\arguments{
 \item{x}{Numeric vector or matrix}
 \item{y}{Optional second numeric vector}
}

\value{
  If two vectors, \code{x} and \code{y}, are provided, the output is a
  matrix with two columns, with the quantile normalized versions of
  \code{x} and \code{y}.

  If \code{y} is missing, \code{x} should be a matrix, in which case the
  output is a matrix of the same dimensions with the columns quantile
  normalized with respect to each other.
}

\details{
  We sort the columns, take averages across rows, and then plug the
  averages back into the respective positions.  The marginal
  distributions in the columns are thus forced to be the same.
}


\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
z <- rmvn(10000, mu=c(0,5,10), V = rbind(c(1,0.5,0.5),c(0.5,1,0.5),c(0.5,0.5,1)))

pairs(z)

par(mfrow=c(3,1))
for(i in 1:3) hist(z[,i], breaks=seq(min(z), max(z), length=300))

zn <- normalize(z)
for(i in 1:3) hist(zn[,i], breaks=seq(min(zn), max(zn), length=300))

pairs(zn)
}

\keyword{utilities}
