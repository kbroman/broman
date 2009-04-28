\name{attachfile}
\alias{attachfile}

\title{Attach a workspace}

\description{
  Attach a workspace for a directory name of a particular form.
}

\usage{
attachfile(i,stem="perm",end=".RData",fixdig=TRUE, verbose=TRUE)
}

\arguments{
  \item{i}{An integer or character string.}
  \item{stem}{Initial part of name.}
  \item{end}{Last part of name.}
  \item{fixdig}{If TRUE and \code{i} is an integer < 10, append a 0 to i.}
  \item{verbose}{If true, print a message if the file can't be loaded.}
}

\details{
  This function attaches the workspace \code{paste(stem,i,end,sep="")},
  possibly adding a 0 before i if i < 10.
}

\value{
  TRUE/FALSE according to whether the file exists (and so the function worked).
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
\dontrun{for(i in 1:5) attachfile(i)}
}

\seealso{ \code{\link[broman]{loadwork}},
  \code{\link[broman]{attachwork}}, \code{\link[broman]{loadfile}}}

\keyword{IO}
