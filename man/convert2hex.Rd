\name{convert2hex}
\alias{convert2hex}
\alias{dec2hex}

\title{Convert to hex}

\description{
  Convert a number to hexidecimal notation.
}

\usage{
convert2hex(d)
dec2hex(d)
}

\arguments{
 \item{d}{A number.}
}

\value{
  A character string; the input in hex.
}

\details{
  Nothing important to say here.
}

\author{Karl W Broman, \email{kbroman@biostat.wisc.edu} }

\examples{
convert2hex(333)
dec2hex(333)
dec2hex(333) == "14D"
dec2hex(0:30)
}

\seealso{ \code{\link{hex2dec}} }

\keyword{manip}
