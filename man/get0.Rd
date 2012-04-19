\name{get0}
\alias{get0}
\alias{get.}

\title{ get with paste }


\description{
  Calls \code{\link[base]{paste}} then \code{\link[base]{get}}
}
\usage{
get0(...)
get.(...)
}

\arguments{
  \item{\dots}{Vector of character strings.}
}
\details{
  There's not much to these functions.  \code{get0} uses \code{\link[base]{paste0}}
  and so combines its arguments with \code{sep=""}.   \code{get.} uses
  \code{\link[base]{paste}} with \code{sep="."}.
}
\value{
The object found.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }


\seealso{
  \code{\link[base]{get}}, 
  \code{\link[base]{paste}}, 
  \code{\link[base]{paste0}} 
}
\examples{
xy <- 3
x.y <- 18.3

get0("x", "y")
get.("x", "y")
}

\keyword{data}
