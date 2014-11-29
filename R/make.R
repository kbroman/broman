#' Run make within a package directory
#'
#' Run make within a package directory
#'
#' @param pkg package description, can be path or package name.
#' See \code{\link[devtools]{as.package}} for more information
#'
#' @param makefile File name of makefile
#'
#' @param quiet If TRUE suppresses output from this function
#'
#' @export
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
    function(pkg = ".", makefile="Makefile", quiet=FALSE)
{
    pkg <- devtools::as.package(pkg)

    if(!quiet) message("Making ", pkg$package)

    fileflag <- ifelse(makefile == "" || makefile=="Makefile" || makefile == "makefile", "", paste("-f", makefile))

    system(paste("cd", pkg$path, "; make", fileflag),
           ignore.stdout=quiet, ignore.stderr=quiet,
           intern=FALSE)
}
