\name{h}
\alias{h}

\title{View html version of help file}

\description{
  View the html version of a help file while running R via ESS within emacs.
}

\usage{
h(\dots)
}

\arguments{
  \item{\dots}{Help topics.}
}

\details{
  This just calls the function \code{\link[utils]{help}} using the
  argument \code{htmlhelp=TRUE}.

}

%\value{
%  No return value.
%}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
h(read.cross)
}

\seealso{ \code{\link[utils]{help}}, \code{\link[utils]{help.start}} }

\keyword{documentation}
