\name{chisq}
\alias{chisq}

\title{Chi-square test by simuation for a two-way table}

\description{
  Calculate a p-value for a chi-square test by Monte Carlo simulation.
}

\usage{
chisq(tab, n.sim=1000)
}

\arguments{
 \item{tab}{A matrix of counts.}
 \item{n.sim}{Number of samples of permuted tables to consider.}
}

\details{
  This is like the function \code{\link[ctest]{chisq.test}}, but
  calculates an approximate P-value rather than refering to
  asymptotics.  This will be better for large, sparse tables.
}

\value{
  A single number: the P-value testing independence of rows and columns
  in the table.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
TeaTasting <- matrix(c(3,1,1,3),nrow=2)
chisq(TeaTasting,1000)
}

\seealso{ \code{\link[stats]{chisq.test}},
  \code{\link[stats]{fisher.test}}, \code{\link[broman]{fisher}} }

\keyword{htest}
