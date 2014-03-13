#' Run make within a package directory
#'
#' Run make within a package directory
#'
#' @param pkg package package description, can be path or package name.
#' See \code{\link[devtools]{as.package}} for more information
#'
#' @param quiet If TRUE suppresses output from this function
#'
#' @export
#' @importFrom devtools as.package 
#' @return Exit value from \code{\link[base]{system}} with \code{intern=FALSE}
#'
#' @examples
#' \dontrun{make() # run make within working directory
#' make("/path/to/mypackage") # run make within /path/to/mypackage
#' }
#'
#' @seealso \code{\link[devtools]{load_all}}
#' @keywords utilities
make <-
function(pkg = ".", quiet=FALSE)
{
  pkg <- as.package(pkg)

  if(!quiet) message("Making ", pkg$package)

  system(paste("cd", pkg$path, "; make"),
         ignore.stdout=quiet, ignore.stderr=quiet,
         intern=FALSE)
}
