\name{trilines}
\alias{trilines}

\title{Plot lines within a Holmans triangle}

\description{
  Plot lines within a Holmans triangle (an equilateral triangle used to depict
  trinomial distributions).
}

\usage{
trilines(x, \dots)
}			    

\arguments{
  \item{x}{A matrix with three rows, each column being a trinomial
  distribution.  Lines between these points are plotted.}
  \item{\dots}{Passed to \code{\link[graphics]{lines}}.}
}

\details{
  Plot of an equilateral triangle, in order to depict trinomial
  distributions.  A trinomial distribution (that is, a trio of
  non-negative numbers that add to 1) is equated to a point in the
  triangle through the distances to the three sides.  This makes use of
  the fact that for any point in an equilateral triangle, the sum of the
  distances to the three sides is constant.  

  First use \code{\link{triplot}} to first plot the equilateral triangle. 
}

\value{The (x,y) coordinates of the endpoints of the lines plotted.}

\examples{
triplot()

x <- cbind(c(0.9, 0.05, 0.05), c(0.8, 0.1, 0.1), c(0.1, 0.9, 0), c(0, 0.9, 0.1))

tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)
trilines(x, lwd=2, col="orange")

y <- cbind(c(0.05, 0.05, 0.9), c(0.25, 0.25, 0.5))
triarrow(y, col="blue", lwd=2, len=0.1)
}

\author{Karl W Broman, \email{kbroman@biostat.wisc.edu} }

\seealso{ \code{\link{triplot}}, \code{\link{tripoints}},
  \code{\link{triarrow}} }

\references{
Holmans P. (1993) Asymptotic properties of affected-sib-pair linkage
analysis. \emph{Amer J Hum Genet} \bold{52}, 362--374. 
}
  

\keyword{hplot}
