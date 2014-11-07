# errors2pushbullet
#' Send further errors to pushbullet
#'
#' Set options to use RPushbullet to use pushbullet to push
#' notifications of any error messages.
#'
#' @param deviceind Numeric index for device to which errors will be
#' pushed; passed to \code{\link[RPushbullet]{pbPost}}.
#'
#' @export
#' @examples
#' \dontrun{errors2pushbullet()}
#' @keywords utilities
errors2pushbullet <-
function(deviceind=1) {
    library(RPushbullet)
    options(error = function() RPushbullet::pbPost("note", "Error", geterrmessage(), deviceind=deviceind) )
}
