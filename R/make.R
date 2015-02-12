#' Run make within a package directory
#'
#' Run make within a package directory
#'
#' @param pkg Path to directory containing the GNU Make file, or an
#' Rpackage description, which can be a path or a package name. (See
#' \code{\link[devtools]{as.package}} for more information.)
#'
#' @param makefile File name of makefile.
#'
#' @param target Optional character string specifying the target.
#'
#' @param quiet If TRUE suppresses output from this function.
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
    function(pkg = ".", makefile="Makefile", target="", quiet=FALSE)
{
    # try to treat pkg as devtools treats a package
    #     but if devtools throws an error, just treat it as a string
    pkgpath <- tryCatch(devtools::as.package(pkg)$path, error=function(cond) pkg)

    if(!quiet) message("Making ", pkgpath)

    # include -f argument only if makefile is not obvious
    fileflag <- ifelse(makefile == "" || makefile=="Makefile" || makefile == "makefile", "", paste("-f", makefile))

    system(paste("cd", pkgpath, "; make", fileflag, target),
           ignore.stdout=quiet, ignore.stderr=quiet,
           intern=FALSE)
}
