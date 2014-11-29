#  attachfile
#'
#' Attach a workspace
#'
#' Attach a workspace for a directory name of a particular form.
#'
#' @param i An integer or character string.
#'
#' @param stem Initial part of name.
#'
#' @param end Last part of name.
#'
#' @param fixdig If TRUE, pad \code{i} with 0's until file exists (up to maxdig)
#'
#' @param maxdig Maximum number of 0's to add
#'
#' @param verbose If true, print a message if the file can't be loaded.
#'
#' @details
#' This function attaches the workspace \code{paste(stem,i,end,sep="")},
#'   possibly adding 0's before i
#'
#' @export
#'
#' @return
#' TRUE/FALSE according to whether the file was attached.
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' \dontrun{for(i in 1:5) attachfile(i)}
#'
#' @seealso
#' \code{\link{loadfile}}
#'
#' @keywords
#' IO
attachfile <-
    function(i,stem="perm",end=".RData",fixdig=TRUE, maxdig=5, verbose=TRUE)
{
    if(missing(i) || is.null(i)) i <- ""

    attach_or_load(i, stem, end, fixdig, maxdig, verbose, "attach")
}

#  loadfile
#'
#' Load a workspace
#'
#' Load a workspace for a directory name of a particular form.
#'
#' @param i An integer or character string.
#'
#' @param stem Initial part of name.
#'
#' @param end Last part of name.
#'
#' @param fixdig If TRUE, pad \code{i} with 0's until file exists (up to maxdig)
#'
#' @param maxdig Maximum number of 0's to add
#'
#' @param verbose If true, print a message if the file can't be loaded.
#'
#' @details
#' This function loads the workspace \code{paste(stem,i,end,sep="")},
#'   possibly adding 0's before i
#'
#' @export
#'
#' @return
#' TRUE/FALSE according to whether the file was loaded.
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' \dontrun{for(i in 1:5) loadfile(i)}
#'
#' @seealso
#' \code{\link{attachfile}}
#'
#' @keywords
#' IO
loadfile <-
    function(i,stem="perm",end=".RData",fixdig=TRUE, maxdig=5, verbose=TRUE)
{
    if(missing(i) || is.null(i)) i <- ""

    attach_or_load(i, stem, end, fixdig, maxdig, verbose, "load")
}

attach_or_load <-
    function(i,stem="perm",end=".RData",fixdig=TRUE, maxdig=5, verbose=TRUE,
             what=c("attach", "load"))
{
    if(missing(i)) i <- ""
    what <- match.arg(what)

    if(fixdig) file <- find_filename_padding(i, stem, end, maxdig)
    else file <- paste0(stem, i, end)

    if(!is.null(file) && file.exists(file)) {
        switch(what,
               attach=attach(file),
               load=load(file, .GlobalEnv))
        result <- TRUE
    }
    else {
        if(verbose) warning("Couldn't attach ", file)
        result <- FALSE
    }
    invisible(result)
}

find_filename_padding <-
    function(i, stem="perm", end=".RData", maxdig=5)
{
    for(j in 0:maxdig) {
        file <- paste0(stem, i, end)
        if(file.exists(file)) break

        i <- paste0("0", i)
    }

    if(file.exists(file)) return(file)
    NULL
}
