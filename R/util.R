######################################################################
# print the installed version of R/broman
######################################################################
#  bromanversion
#'
#' Installed version of R/broman
#'
#' Print the version number of the currently installed version of R/broman.
#'
#' @export
#' @return
#' A character string with the version number of the currently installed
#'   version of R/broman.
#'
#' @examples
#' bromanversion()
#'
#' @keywords
#' print
bromanversion <-
    function()
{
    version <- unlist(utils::packageVersion("broman"))

    # make it like #.#-#
    paste(c(version,".","-")[c(1,4,2,5,3)], collapse="")
}
