#  loadwork
#'
#' Load a workspace
#'
#' Load a workspace for a directory name of a particular form.
#'
#' @param i An integer or character string.
#'
#' @param dir Directory name.
#'
#' @param verbose If true, print a message if the file can't be loaded.
#'
#' @details
#' This function loads the workspace \code{paste(dir,i,"/.RData",sep="")}.
#'
#' @export
#'
#' @return
#' TRUE/FALSE according to whether the file exists (and so the function worked).
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' \dontrun{for(i in 1:5) loadwork(i)}
#'
#' @seealso
#' \code{\link{attachwork}},
#'   \code{\link{attachfile}},   \code{\link{loadfile}}
#'
#' @keywords
#' IO
loadwork <-
function(i="",dir="Work", verbose=TRUE) {
  file <- paste(dir,i,"/.RData",sep="")
  if(file.exists(file)) {
    load(file, .GlobalEnv)
    result <- TRUE
  }
  else {
    if(verbose) warning("Couldn't load ", file)
    result <- FALSE
  }
  invisible(result)
}

#  attachwork
#'
#' Attach a workspace
#'
#' Attach a workspace for a directory name of a particular form.
#'
#' @param i An integer or character string.
#'
#' @param dir Directory name.
#'
#' @param verbose If true, print a message if the file can't be loaded.
#'
#' @details
#' This function attaches the workspace \code{paste(dir,i,"/.RData",sep="")}.
#'
#' @export
#'
#' @return
#' TRUE/FALSE according to whether the file exists (and so the function worked).
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' \dontrun{for(i in 1:5) attachwork(i)}
#'
#' @seealso
#' \code{\link{loadwork}},
#'   \code{\link{attachfile}}, \code{\link{loadfile}}
#'
#' @keywords
#' IO
attachwork <-
function(i="",dir="Work", verbose=TRUE) {
  file <- paste(dir,i,"/.RData",sep="")
  if(file.exists(file)) {
    attach(file)
    result <- TRUE
  }
  else {
    if(verbose) warning("Couldn't attach ", file)
    result <- FALSE
  }
  invisible(result)
}
  
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
#' @param fixdig If TRUE and \code{i} is an integer < 10, append a 0 to i.
#'
#' @param verbose If true, print a message if the file can't be loaded.
#'
#' @details
#' This function attaches the workspace \code{paste(stem,i,end,sep="")},
#'   possibly adding a 0 before i if i < 10.
#'
#' @export
#'
#' @return
#' TRUE/FALSE according to whether the file exists (and so the function worked).
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' \dontrun{for(i in 1:5) attachfile(i)}
#'
#' @seealso
#' \code{\link{loadwork}},
#'   \code{\link{attachwork}}, \code{\link{loadfile}}
#'
#' @keywords
#' IO
attachfile <-
function(i,stem="perm",end=".RData",fixdig=TRUE, verbose=TRUE)
{
  if(missing(i)) i <- ""
  else if(fixdig && is.numeric(i) && i<10) i <- paste("0",i,sep="")
  file <- paste(stem,i,end,sep="")
  if(file.exists(file)) {
    attach(file)
    result <- TRUE
  }
  else {
    if(verbose) warning("Couldn't attach ", file)
    result <- FALSE
  }
  invisible(result)
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
#' @param fixdig If TRUE and \code{i} is an integer < 10, append a 0 to i.
#' 
#' @param verbose If true, print a message if the file can't be loaded.
#'
#' @details
#' This function loads the workspace \code{paste(stem,i,end,sep="")},
#'   possibly adding a 0 before i if i < 10.
#'
#' @export
#'
#' @return
#' TRUE/FALSE according to whether the file exists (and so the function worked).
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' \dontrun{for(i in 1:5) loadfile(i)}
#'
#' @seealso
#' \code{\link{loadwork}},
#'   \code{\link{attachwork}}, \code{\link{attachfile}}
#'
#' @keywords
#' IO
loadfile <-
function(i,stem="perm",end=".RData",fixdig=TRUE, verbose=TRUE)
{
  if(missing(i)) i <- ""
  else if(fixdig && is.numeric(i) && i<10) i <- paste("0",i,sep="")
  file <- paste(stem,i,end,sep="")
  invisible()
  if(file.exists(file)) {
    load(file, .GlobalEnv)
    result <- TRUE
  }
  else {
    if(verbose) warning("Couldn't attach ", file)
    result <- FALSE
  }
  invisible(result)
}
