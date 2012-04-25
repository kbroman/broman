\name{revrainbow}
\alias{revrainbow}

\title{Create vector of colors from blue to red}


\description{
  Calls \code{\link[grDevices]{rainbow}} then \code{\link[base]{rev}}
}
\usage{
revrainbow(n=256, \dots)
}

\arguments{
  \item{n}{Number of colors.}
  \item{\dots}{Passed to \code{\link[grDevices]{rainbow}}.}
}
\details{
  There's not much to this. It's just \code{rev(rainbow(start=0, end=2/3, ...))}.
}

\value{
Vector of colors, from blue to red.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }


\seealso{
  \code{\link[base]{rev}}, 
  \code{\link[grDevices]{rainbow}} 
}
\examples{
x <- matrix(rnorm(100), ncol=10)
image(x, col=revrainbow())
}

\keyword{color}
