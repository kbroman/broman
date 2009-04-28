######################################################################
#
# loadwork.R
#
# copyright (c) 2008, Karl W Broman
# Last revised:  Sep, 2008
# First written: Aug, 2004
# Licensed under the GNU General Public License version 2 (June, 1991)
# 
# Part of the R/broman package
# Contains: loadwork, attachwork, attachfile
#
######################################################################

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

# end of loadwork.R
