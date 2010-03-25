\name{nqrank}
\alias{nqrank}

\title{Convert data to normal quantiles}

\description{
  Convert data to the normal quantiles corresponding to the ranks.
}

\usage{
nqrank(x, jitter=FALSE)
}

\arguments{
 \item{x}{A numeric vector; NAs and Infs are allowed.}
 \item{jitter}{If TRUE, randomly jitter the ties.}
}

\details{
  The data are converted to ranks, and then to the corresponding normal
  quantiles, preserving the mean and SD.  NAs are ignored and returned
  as NAs.  Infs are taken to be 10 more than the largest finite value;
  -Infs are taken to be 10 less than the smallest finite value.
}

\value{
  A vector of the same length as the input, with NAs retained.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
x <- runif(30)
nqrank(x)
}

\seealso{ \code{\link[stats]{qnorm}}, \code{\link[base]{rank}} }

\keyword{univar}
