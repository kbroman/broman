######################################################################
# runningmean.R
#
# Karl W Broman
# last modified Dec, 2011
# first written Sep, 2005
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
# Contains: runningmean, runningratio
######################################################################

######################################################################
# get running mean, sum or median within a specified window
######################################################################
runningmean <-
function(pos, value, at, window=1000, what=c("mean","sum", "median", "sd"))
{
  what <- which(c("sum","mean","median","sd")==match.arg(what))
  
  n <- length(pos)
  if(length(value) != n)
    stop("pos and value must have the same length\n")

  if(missing(at)) # if missing 'at', use input 'pos'
    at <- pos

  # check that pos is sorted
  if(any(diff(pos) < 0)) { # needs to be sorted
    o <- order(pos)
    pos <- pos[o]
    value <- value[o]
  }
    
  # check that pos is sorted
  if(any(diff(at) < 0)) { # needs to be sorted
    o.at <- order(at)
    at <- at[o.at]
    reorderresult <- TRUE
  }
  else reorderresult <- FALSE
    
  n.res <- length(at)

  z <- .C("R_runningmean",
          as.integer(n),
          as.double(pos),
          as.double(value),
          as.integer(n.res),
          as.double(at),
          z=as.double(rep(0,n.res)),
          as.double(window),
          as.integer(what),
          PACKAGE="broman")$z
  
  if(reorderresult) 
    z <- z[match(1:length(at), o.at)]
  
  z
}


######################################################################
# runningratio
#
# take sum(numerator)/sum(denominator) in sliding window
######################################################################
runningratio <-
function(pos, numerator, denominator, at, window=1000)
{
  n <- length(pos)
  if(length(numerator) != n || length(denominator) != n)
    stop("pos, numerator and denominator must all be the same length\n")

  if(missing(at)) # if missing 'at', use input 'pos'
    at <- pos

  # check that pos is sorted
  if(any(diff(pos) < 0)) { # needs to be sorted
    o <- order(pos)
    pos <- pos[o]
    value <- value[o]
  }
    
  # check that pos is sorted
  if(any(diff(at) < 0)) { # needs to be sorted
    o.at <- order(at)
    at <- at[o.at]
    reorderresult <- TRUE
  }
  else reorderresult <- FALSE
    
  n.res <- length(at)

  z <- .C("R_runningratio",
          as.integer(n),
          as.double(pos),
          as.double(numerator),
          as.double(denominator),
          as.integer(n.res),
          as.double(at),
          z=as.double(rep(0,n.res)),
          as.double(window),
          PACKAGE="broman")$z

  if(reorderresult)
    z <- z[match(1:length(z), o.at)]

  z
}

# end of runningmean.R
