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
#' @examples
#' \dontrun{errors2pushbullet()}
#' @keywords utilities
#' @seealso \code{\link{stop_sending_errors}}
errors2pushbullet <-
    function(recipients) {
        load_pushbullet()

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

# errors2pushbullet
#' Stop sending errors to pushbullet
#'
#' Clear the \code{error} option, so that error notifications are no
#' longer sent to pushbullet.
#'
#' @export
#' @examples
#' \dontrun{stop_sending_errors()}
#' @keywords utilities
#' @seealso \code{\link{errors2pushbullet}}
stop_sending_errors <-
    function()
{
    options(error = NULL)
}


is_pushbullet_loaded <-
    function()
{
    if(is.null(getOption("rpushbullet.key")))
        return(FALSE)

    TRUE
}

# the following is a modified version of code from
#     https://github.com/eddelbuettel/rpushbullet/blob/master/R/init.R
load_pushbullet <-
    function()
{
    if(!is_pushbullet_loaded()) {
        Sys.setenv(Rbroman_pushbullet_loaded = TRUE)

        dotfile <- "~/.rpushbullet.json"
        if (file.exists(dotfile)) {
            message("Loading ", dotfile, " for RPushbullet")
            pb <- jsonlite::fromJSON(dotfile)
            assign("pb", pb, envir=new.env(parent=emptyenv()))
            options("rpushbullet.key" = pb[["key"]])
            options("rpushbullet.devices" = pb[["devices"]])
            options("rpushbullet.names" = pb[["names"]])
            ## defaultdevice is an optional entry, with fallback value of 0
            options("rpushbullet.defaultdevice" =
                    if ("defaultdevice" %in% names(pb)) pb[["defaultdevice"]] else 0)
            ## these are for testing
            options("rpushbullet.testemail" =
                    if ("testemail" %in% names(pb)) pb[["testemail"]] else character())
            options("rpushbullet.testchannel" =
                    if ("testchannel" %in% names(pb)) pb[["testchannel"]] else character())
        }
        else stop("Cannot load ~/.rpushbullet.json")
    }
}

# done
#' Send a short message via RPushbullet.
#'
#' Send a short message via RPushbullet, to be used to indicate that
#' some R job is complete.
#' #'
#' @param message A character string with a message.
#' (passed to \code{\link[RPushbullet]{pbPost}}.)
#' @param recipients A character or numeric vector indicating the
#' devices this post should go to. If missing, the default device is
#' looked up from an optional setting, and if none has been set the
#' push is sent to all devices. (passed to
#' \code{\link[RPushbullet]{pbPost}}.)
#'
#' @export
#' @examples
#' \dontrun{done("Your R job is complete.")}
#' @keywords utilities
# got this name from Ian Kyle; see http://bit.ly/IanKyle_systemdone
done <-
    function(message="R is done", recipients)
{
    load_pushbullet()

    if(!missing(recipients) && !is.null(recipients))
        RPushbullet::pbPost("note", message, recipients=recipients)
    else
        RPushbullet::pbPost("note", message)
}

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
#' \dontrun{note("Hello.")}
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

# pushbullet_devices
#' Grab info on Pushbullet devices.
#'
#' Get names and identifiers of Pushbullet devices.
#'
#' @export
#' @return
#' data frame with nickname, model, and iden for active devices.
pushbullet_devices <-
    function()
{
    dev <- RPushbullet::pbGetDevices()$devices
    dev <- dev[vapply(dev, '[[', TRUE, 'active')]

    data.frame(nickname=vapply(dev, '[[', 'string', 'nickname'),
               iden=vapply(dev, '[[', 'string', 'iden'),
               model=vapply(dev, '[[', 'string', 'model'),
               stringsAsFactors=FALSE)
}
