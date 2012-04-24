\name{paste.}
\alias{paste.}

\title{ paste with dot separator }


\description{
  Calls \code{\link[base]{paste}} with \code{sep="."}.
}
\usage{
paste.(...)
}

\arguments{
  \item{\dots}{Passed to paste.}
}
\details{
  There's not much to this function.  It just is
  \code{\link[base]{paste}} with \code{sep=""}, 'cause I'm lazy.
}
\value{
A character string or vector of character strings.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }


\seealso{
  \code{\link[base]{paste}}, 
  \code{\link[base]{paste0}} 
}
\examples{
x <- 3
y <- 4

paste.(x, y)
}

\keyword{character}
