\name{venn}
\alias{venn}

\title{Plot to-scale Venn diagram}

\description{
  Plot a Venn diagram (with two groups), to scale, either with circles
  or with squares.
}

\usage{
venn(setA=50, setB=50, both=25, method=c("circle","square"),
     labels=c("A","B"), col=c("blue","red"))
}			    

\arguments{
  \item{setA}{Total area of set A.}
  \item{setB}{Total area of set B.}
  \item{both}{Area of intersection of sets A and B.}
  \item{method}{Indicates whether to plot circles or squares.}
  \item{labels}{Labels for the two sets.  (\code{NULL} for no labels.)}
  \item{col}{Colors of the two sets.}
}

\details{
  Plots a to-scale Venn diagram with two sets, so that the relative
  areas of the two sets and their intersection are exact.
}

\value{None.}

\examples{
venn(setA=86, setB=1622, both=10)
venn(setA=86, setB=1622, both=10, method="square")
}

\author{Karl W Broman, \email{kbroman@biostat.wisc.edu} }

\keyword{hplot}
