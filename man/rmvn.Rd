\name{rmvn}
\alias{rmvn}

\title{Simulate multivariate normal}

\description{
  Simulate from a multivariate normal distribution.
}

\usage{
rmvn(n, mu=0, V=matrix(1))
}

\arguments{
 \item{n}{Number of simulation replicates.}
 \item{mu}{Mean vector.}
 \item{V}{Variance-covariance matrix.}
}

\details{
  Uses the Cholesky decomposition of the matrix \code{V}, obtained by
  \code{\link[base]{chol}}.  
}

\value{
  A matrix of size n x \code{length(mu)}.  Each row corresponds to a
  separate replicate.
}

%\references{
%}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
x <- rmvn(100, c(1,2),matrix(c(1,1,1,4),ncol=2))
}

\seealso{ \code{\link[base]{rnorm}} }

\keyword{datagen}
