# note
#' Send a note to pushbullet
#'
#' Even simpler interface for \code{\link[RPushbullet]{pbPost}}
#' to send a bit of text to pushbullet.
#'
#' @param title The title of the note (could be the whole thing).
#' @param recipients A character or numeric vector indicating the
#' devices this post should go to. If missing, the default device is
#' looked up from an optional setting, and if none has been set the
#' push is sent to all devices. (passed to
#' \code{\link[RPushbullet]{pbPost}}.)
#' @param body The body of the note (by default, empty)
#'
#' @export
#' @examples
#' \dontrun{errors2pushbullet()}
#' @keywords utilities
note <-
    function(title, recipients, body="")
{
    load_pushbullet()

    if(!missing(recipients) && !is.null(recipients))
        RPushbullet::pbPost("note", title=title, body=body, recipients=recipients)
    else
        RPushbullet::pbPost("note", title=title, body=body)
}
