\name{myround}
\alias{myround}

\title{Round a number, preserving extra 0's}

\description{
  Round a number, preserving extra 0's.
}

\usage{
myround(x, digits=1)
}

\arguments{
 \item{x}{Number to round.}
 \item{digits}{Number of digits past the decimal point to keep.}
}

\details{
  Rounds a number, keeping extra 0's.
}

\value{
  A vector of character strings.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
myround(51.01, 3)
myround(0.199, 2)
}

\keyword{utilities}
