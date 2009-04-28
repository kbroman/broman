######################################################################
# runningmean.R
#
# Karl W Broman
# last modified Sep, 2007
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
function(pos, value, window=1000, what=c("mean","sum", "median"))
{
  what <- which(c("sum","mean","median")==match.arg(what))
  
  n <- length(pos)
  if(length(value) != n)
    stop("pos and value must have the same length\n")

  .C("R_runningmean",
     as.integer(n),
     as.double(pos),
     as.double(value),
     z=as.double(rep(0,n)),
     as.double(window),
     as.integer(what),
     PACKAGE="broman")$z
}


######################################################################
# runningratio
#
# take sum(numerator)/sum(denominator) in sliding window
######################################################################
runningratio <-
function(pos, numerator, denominator, window=1000)
{
  n <- length(pos)
  if(length(numerator) != n || length(denominator) != n)
    stop("pos, numerator and denominator must all be the same length\n")

  .C("R_runningratio",
     as.integer(n),
     as.double(pos),
     as.double(numerator),
     as.double(denominator),
     z=as.double(rep(0,n)),
     as.double(window),
     PACKAGE="broman")$z
}

# end of runningmean.R
