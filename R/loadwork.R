######################################################################
#
# loadwork.R
#
# copyright (c) 2008, Karl W Broman
# Last revised:  Sep, 2008
# First written: Aug, 2004
#
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License,
#     version 3, as published by the Free Software Foundation.
# 
#     This program is distributed in the hope that it will be useful,
#     but without any warranty; without even the implied warranty of
#     merchantability or fitness for a particular purpose.  See the GNU
#     General Public License, version 3, for more details.
# 
#     A copy of the GNU General Public License, version 3, is available
#     at http://www.r-project.org/Licenses/GPL-3
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
