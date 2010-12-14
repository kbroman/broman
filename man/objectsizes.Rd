\name{objectsizes}
\alias{objectsizes}

\title{Calculate sizes of all objects in workspace}

\description{
  Calculate the sizes of all of the objects in one's workspace.
}

\usage{
objectsizes(obj, sortbysize=TRUE)
}

\arguments{
  \item{obj}{Vector of object names.  If missing, we pull out all object
  names.}
  \item{sortbysize}{If TRUE, sort the objects from smallest to largest.}
}

\details{
  Calls \code{\link[utils]{object.size}} repeated to get the size of a
  list of objects.
}

\value{
  A data frame with the only column being the size of each object in
  megabytes (Mb).  The row names are the names of the objects.
}

\author{Karl W Broman \email{kbroman@biostat.wisc.edu} }

\examples{
print(output <- objectsizes())
\dontrun{sum(output)}
}

\seealso{ \code{\link[utils]{object.size}},
  \code{\link[base]{objects}} }

\keyword{utilities}
