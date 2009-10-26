\name{fisher}
\alias{fisher}

\title{Fisher's exact test for a two-way table}

\description{
  Performs a sampling version of Fisher's exact test for a two-way
  contingency table.
}

\usage{
fisher(tab, n.sim=1000)
}

\arguments{
 \item{tab}{A matrix of counts.}
 \item{n.sim}{Number of samples of permuted tables to consider.}
}

\details{
  This is like the function \code{\link[stats]{fisher.test}}, but
  calculates an approximate P-value rather than performing a complete
  enumeration.  This will be better for large, sparse tables.
}

\value{
  A single number: the P-value testing independence of rows and columns
  in the table.
}

\references{
  Fisher, R. A. (1935). The logic of inductive inference. \emph{J.
  Roy. Stat. Soc.} A \bold{98}, 39--54.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
TeaTasting <- matrix(c(3,1,1,3),nrow=2)
fisher(TeaTasting,1000)
}

\seealso{ \code{\link[stats]{chisq.test}},
  \code{\link[stats]{fisher.test}}, \code{\link{chisq}} }

\keyword{htest}
