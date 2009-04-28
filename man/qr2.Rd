\name{qr2}
\alias{qr2}

\title{The QR decomposition of a matrix}

\description{
  Computes the QR decomposition of a matrix.
}

\usage{
qr2(x, tol=1e-7)
}

\arguments{
 \item{x}{A matrix whose QR decomposition is to be computed.}
 \item{tol}{The tolerance for detecting linear dependencies in the
   columns of \code{x}.}
}

\details{
  Calls the function \code{\link[base]{qr}} and returns
  less compact but more understandable output.
}

\value{
  A list of two matrices: Q and R.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
hilbert <- function(n) { i <- 1:n; 1/outer(i-1,i,"+") }
h5 <- hilbert(5);
qr2(h5)
}

\seealso{ \code{\link[base]{qr}} }

\keyword{algebra}
