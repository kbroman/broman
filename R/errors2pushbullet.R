# errors2pushbullet
#' Send further errors to pushbullet
#'
#' Set options to use RPushbullet to use pushbullet to push
#' notifications of any error messages.
#'
#' @param recipients A character or numeric vector indicating the
#' devices this post should go to. If missing, the default device is
#' looked up from an optional setting, and if none has been set the
#' push is sent to all devices. (passed to
#' \code{\link[RPushbullet]{pbPost}}.)
#'
#' @export
#' @import RPushbullet
#' @examples
#' \dontrun{errors2pushbullet()}
#' @keywords utilities
errors2pushbullet <-
    function(recipients) {
        if(!missing(recipients) && !is.null(recipients))
            options(error = function() {
                RPushbullet::pbPost("note", "Error", geterrmessage(), recipients=recipients)
                if(!interactive()) stop(geterrmessage())
            } )
        else
            options(error = function()  {
                RPushbullet::pbPost("note", "Error", geterrmessage())
                if(!interactive()) stop(geterrmessage())
            } )
    }
