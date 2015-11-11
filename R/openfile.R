#  openfile
#'
#' Open a file
#'
#' Open a file using \code{\link[base]{system}} and \code{open}.
#'
#' @param file File name (character string)
#'
#' @details I'd thought that to open a file you'd use \code{open} in
#' MacOS and \code{start} in Windows, but \code{system("start
#' myfile.pdf")} doesn't work in Windows, but rather
#' \code{system("open myfile.pdf")} does, so here we're just using
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
    system( paste("open", file) )
}
