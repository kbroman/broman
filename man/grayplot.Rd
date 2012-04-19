\name{grayplot}
\alias{grayplot}

\title{Plot with a gray background}

\description{Like the plot function, but using a gray background just
  for the plot regin.}

\usage{
grayplot(x, y, ..., type="p",
         hlines, hlines.col = "white", hlines.lty = 1, hlines.lwd = 1,
         vlines, vlines.col = "white", vlines.lty = 1, vlines.lwd = 1,
         xat, yat, bgcolor = "gray80")
}

\arguments{
  \item{x}{Coordinates of points in the plot}
  \item{y}{Corrdinates of points in the plot (optional)}
  \item{\dots}{Optional graphics arguments}
  \item{type}{Plot type (points, lines, etc.)}
  \item{hlines}{Locations of horizontal grid lines}
  \item{hlines.col}{Colors of horizontal grid lines}
  \item{hlines.lty}{Line type of horizontal grid lines}
  \item{hlines.lwd}{Line width of horizontal grid lines}
  \item{vlines}{Locations of vertical grid lines}
  \item{vlines.col}{Colors of vertical grid lines}
  \item{vlines.lty}{Line type of vertical grid lines}
  \item{vlines.lwd}{Line width of vertical grid lines}
  \item{xat}{Locations for x-axis labels}
  \item{yat}{Locations for y-axis labels}
  \item{bgcolor}{Background color}
}

\details{
  Calls \code{\link[graphics]{plot}} with \code{type="n"}, then
  \code{\link[graphics]{rect}} to get the background, and then
  \code{\link[graphics]{points}}. 
}

\value{
  None.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\seealso{
  \code{\link[graphics]{plot}}, 
  \code{\link[graphics]{par}}, 
  \code{\link[graphics]{rect}}, 
  \code{\link[graphics]{points}} 
}

\examples{
\dontshow{set.seed(97536917)}
x <- rnorm(100)
y <- x+rnorm(100, 0, 0.7)

grayplot(x, y, col="blue", pch=16)

at <- seq(-3, 3)
grayplot(x, y, col="blue", pch=16, hlines=at, vlines=at)

grayplot(x, col="violet", pch=16, bgcolor="gray90",
         hlines=seq(-4, 4, by=0.5), hlines.lwd=c(3,1),
         vlines=seq(0, 100, by=5), vlines.lwd=c(3,1,1,1))
}

\keyword{graphics}
