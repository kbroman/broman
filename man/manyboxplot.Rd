\name{manyboxplot}
\alias{manyboxplot}

\title{Boxplot-like figure for many groups}

\description{Boxplot-like figure for many groups, with lines connecting
  selected quantiles.}


\usage{
manyboxplot(x, probs = c(0.05, 0.1, 0.25), dotcol = "blue",
           linecol = c("black", "red", "green", "orange"), ...)
}

\arguments{
  \item{x}{Matrix of data, with columns indicating the groups.}

  \item{probs}{Numeric vecotr of probabilities with values in [0,1).
    Quantiles will be symmetric, and the median will always be included.}
  \item{dotcol}{Color for median}
  \item{linecol}{Color for the quantile lines (same length as \code{probs};
    used symmetrically).}
  \item{\dots}{Passed to \code{\link{grayplot}}.}
}

\details{
  Calculates quantiles of the columns of \code{x} and then plots dots or
  lines at median plus lines at a series of quantiles, using
  \code{\link{grayplot}} for the actual plot.
}

\value{
  None.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }


\seealso{ \code{\link{grayplot}} }

\examples{
mu <- c(rnorm(50, 0, 0.3), rnorm(50, 2, 0.3)) # vector of means
x <- t(matrix(rnorm(1000*100, mu), ncol=1000))

manyboxplot(x, c(0.05, 0.25), dotcol=rep(c("blue","green"),c(50,50)),
           ylim=range(x))
}

\keyword{ graphics }
