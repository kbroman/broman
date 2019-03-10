#  openfile
#'
#' Open a file
#'
#' Open a file using [base::system() and `"open"`
#' (well, actually `"start"` on Linux).
#'
#' @param file File name (character string)
#'
#' @details I'd thought that to open a file you'd use `open` in
#'     MacOS and `start` in Windows, but
#'     `system("start myfile.pdf")` doesn't work in Windows, and
#'     rather `system("open myfile.pdf")` does, so here we're
#'     just using `open`, except on Linux where at least on my
#'     system, you can use `"start"`.
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
