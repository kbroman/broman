# errors2pushbullet
#' Send further errors to pushbullet
#'
#' Set options to use RPushbullet to use pushbullet to push
#' notifications of any error messages.
#'
#' @export
#' @examples
#' \dontrun{errors2pushbullet()}
#' @keywords utilities
errors2pushbullet <-
function() {
    library(RPushbullet)
    options(error = function() pbPost("note", "Error", geterrmessage()) )
}
