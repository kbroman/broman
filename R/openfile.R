#  openfile
#'
#' Open a file
#'
#' Open a file using \code{\link[base]{system}} and \code{open} (on
#' Mac) or \code{start} (on Windows).
#'
#' @param file File name (character string)
#'
#' @export
#'
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
    command <- switch(Sys.info()[1],
                      "Darwin"=paste("open", file),
                      "Windows"=paste("start", file),
                      stop("Not sure what to do with OS ", Sys.info()[1], " OS"))
    system(command)
}
