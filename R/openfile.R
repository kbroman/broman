#  openfile
#'
#' Open a file
#'
#' Open a file using \code{\link[base]{system}} and \code{"open"}
#' (well, actually \code{"start"} on Linux).
#'
#' @param file File name (character string)
#'
#' @details I'd thought that to open a file you'd use \code{open} in
#'     MacOS and \code{start} in Windows, but
#'     \code{system("start myfile.pdf")} doesn't work in Windows, and
#'     rather \code{system("open myfile.pdf")} does, so here we're
#'     just using \code{open}, except on Linux where at least on my
#'     system, you can use \code{"start"}.
#'
#' @export
#' @return None.
#'
#' @examples
#' \dontrun{openfile("myplot.pdf")}
#'
#' @keywords
#' IO
openfile <-
    function(file)
{
    # determine OS
    os <- Sys.info()["sysname"]

    command <- ifelse(os=="Linux", "start", "open")

    system( paste(command, file) )
}
