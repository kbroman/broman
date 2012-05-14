\name{paste00}
\alias{paste00}
\alias{paste..}
\alias{paste0.}
\alias{paste.0}

\title{ paste with null or dot as separator and with collapse }


\description{
  Call \code{\link[base]{paste}} with \code{sep="."} or \code{sep=""}
  and \code{collapse=""} or \code{collapse="."}.
}
\usage{
paste00(...)
paste..(...)
paste0.(...)
paste.0(...)
}

\arguments{
  \item{\dots}{Passed to paste.}
}
\details{
  There's not much to these functions.

  \code{paste00(\dots)} is like \code{paste(\dots, sep="", collapse="")}

  \code{paste..(\dots)} is like \code{paste(\dots, sep=".", collapse=".")}

  \code{paste0.(\dots)} is like \code{paste(\dots, sep="", collapse=".")}

  \code{paste.0(\dots)} is like \code{paste(\dots, sep=".", collapse="")}
}
\value{
A character string or vector of character strings.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }


\seealso{
  \code{\link[base]{paste}}, 
  \code{\link[base]{paste0}},
  \code{\link{paste.}}
}
\examples{
x <- c(3, 4)
y <- c(5, 6)

paste00(x, y)
paste..(x, y)
paste0.(x, y)
paste.0(x, y)
}

\keyword{character}
