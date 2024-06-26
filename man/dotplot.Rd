% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dotplot.R
\name{dotplot}
\alias{dotplot}
\title{Dot chart with a gray background}
\usage{
dotplot(group, y, jiggle = NULL, max_jiggle = 0.45, rotate = FALSE, ...)
}
\arguments{
\item{group}{Categorical coordinates for the plot}

\item{y}{Coordinates of points in the plot}

\item{jiggle}{Vector of amounts to jiggle the points horizontally,
or a character string (\code{"fixed"} or \code{"random"})
indicating the jiggling method; see \code{\link[=jiggle]{jiggle()}}.}

\item{max_jiggle}{Maximum jiggle value; passed to \code{\link[=jiggle]{jiggle()}} as argument \code{maxvalue}.}

\item{rotate}{If TRUE, have group as y-axis; default (FALSE) has
group on x-axis.}

\item{...}{Optional graphics arguments}
}
\value{
None.
}
\description{
Like the \code{\link[=grayplot]{grayplot()}} function, but with one axis assumed to be categorical.
}
\details{
Calls \code{\link[=grayplot]{grayplot()}} with special choices of
graphics parameters for the case of categorical x.

If \code{group} is a factor, the order of the groups is as in the
levels. Otherwise, we take \code{sort(unique(group))}. So if you want to
control the order of the levels, make \code{group} a factor with the levels
in the desired order, for example \code{group <- factor(group, levels=unique(group))}.
}
\examples{
\dontshow{set.seed(97536917)}
x <- rnorm(40, c(1,3))
g <- rep(c("A", "B"), 20)
dotplot(g, x)
dotplot(g, x, "fixed")
dotplot(g, x, runif(length(g), -0.25, 0.25))

}
\seealso{
\code{\link[=grayplot]{grayplot()}}, \code{\link[=timeplot]{timeplot()}}
}
\keyword{graphics}
