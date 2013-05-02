\name{xlimlabel}
\alias{xlimlabel}

\title{
Calulate horizontal limit in user coordinates for adding labels
}
\description{
Calculates the x-axis limits when adding (long) labels to a plot
}
\usage{
xlimlabel(x, xlabels, pos=4, offset = 0.5, ...)
}

\arguments{
  \item{x}{numeric vector of horizontal coordinates}
  \item{xlabels}{character vector, specifying text to be written}
  \item{pos}{position specifier for text; values of \code{1}, \code{2},
    \code{3}, and \code{4}, respectively, indicate positions below, to
    the left of, above, and to the right of the coordinates}
  \item{offset}{offset of the label from the coordinate in fractions of
    a character width}
  \item{\dots}{additional text parameters from \code{par}, such as \code{cex}}
}
}
\details{
  See \code{text} for details on \code{pos} and \code{offset}.
}
\value{
Minimum and maximum x-axis limits for adding horizontal text
}
\author{
Aimee Teo Broman
}
\seealso{
\code{text}
}
\examples{
x <- runif(15,-1,1)*10
xlabs <- sapply(sample(1:20,15,replace=TRUE),
         function(a) paste(LETTERS[1:a], collapse=""))

par(mfrow=c(2,1))
## Labels to the left ##
xlims <- xlimlabel(x,xlabs,pos=2)
plot(x,1:length(x),xlim=xlims)
text(x,1:length(x),xlabs,pos=2)

## Labels to the right ##
xlims <- xlimlabel(x,xlabs,pos=4,cex=0.7)
plot(x,1:length(x),xlim=xlims)
text(x,1:length(x),xlabs,pos=4,cex=0.7)
}
