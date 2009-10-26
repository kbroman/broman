\name{perm.test}
\alias{perm.test}

\title{Permutation t-test}

\description{
  Calculates a p-value for a t-test via permutations.
}

\usage{
perm.test(x, y, n.perm=NULL, var.equal=TRUE, pval=TRUE)
}

\arguments{
  \item{x}{A numeric vector.}
  \item{y}{A second numeric vector.}  
  \item{n.perm}{Number of permutations to perform.  If NULL, all
    possible permutations are considered, and an exact p-value is
    calculated.} 
  \item{var.equal}{A logical variable indicating whether to treat the two
    population variances as being equal.}
  \item{pval}{If TRUE, return just the p-value.  If FALSE, return the
  actual permutation results (with the observed statistic as an
  attribute, \code{"tobs"}).}
}

\details{
  This calls the function \code{\link[stats]{t.test}} to calculate a
  t-statistic comparing the vectors \code{x} and \code{y}.  Permutations
  are perfomed to give an exact or approximate conditional p-value.
}

\value{
  If \code{pval=TRUE}, the output is a single number: the P-value
  testing for a difference in the distributions of the populations from
  which \code{x} and \code{y} were drawn.

  If \code{pval=FALSE}, the output is a vector of the t statistics from
  the permutations.  An attributed \code{"tobs"} contains the t
  statistic with the observed data.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
x <- c(43.3, 57.1, 35.0, 50.0, 38.2, 61.2)
y <- c(51.9, 95.1, 90.0, 49.7, 101.5, 74.1) 
perm.test(x,y)
}

\seealso{ \code{\link[stats]{t.test}}, \code{\link{paired.perm.test}} }

\keyword{htest}
