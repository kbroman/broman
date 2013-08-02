\name{qqline2}
\title{qqline for qqplot}
\usage{
qqline2(x, y, probs=c(0.25, 0.75), qtype=7, \dots)
}
\alias{qqline2}

\arguments{
  \item{x}{The first sample}
  \item{y}{The second sample.}
  \item{probs}{numeric vector of length two, representing probabilities.
    Corresponding quantile pairs define the line drawn.}
  \item{qtype}{the \code{type} of quantile computation used in \code{\link{quantile}}.}
  \item{\dots}{graphical parameters.}
}
\description{
  Adds a line to a quantile-quantile plot for two datasets, from \code{\link{stats}[qqplot]}.

  (The available \code{\link[stats]{qqline}} function works mainly for
  \code{\link[stats]{qqnorm}}, with one sample being theoretical quantiles.)
}
\value{
  Intercept and slope of the line.
}
\seealso{
  \code{\link[stats]{qqline}}, \code{\link[stats]{qqplot}}
}
\examples{
x <- rchisq(500, 3)
y <- rgamma(730, 3, 1/2)

qqplot(x, y)
qqline2(x, y)
}
\keyword{hplot}
\keyword{distribution}
