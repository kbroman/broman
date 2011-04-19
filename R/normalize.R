######################################################################
#
# normalize.R
#
# copyright (c) 2005-2011, Karl W Broman
# last modified 19 Apr 2011
# first written 18 Sep 2005
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
# Contains: normalize
#
######################################################################

######################################################################
# normalize:
#  force a matrix of intensities to have columns with the same marginal
#  distributions.  
######################################################################
normalize <-
function(x,y)
{
  if(!missing(y)) x <- cbind(x,y)
  if(is.data.frame(x)) x <- as.matrix(x)

  x[abs(x) == Inf] <- NA
  maxval <- max(x, na.rm=TRUE) + 1
  if(any(is.na(x))) x[is.na(x)] <- maxval+100

  n <- nrow(x)
  p <- ncol(x)

  z <- .C("R_normalize",
          as.integer(n),
          as.integer(p),
          x=as.double(x),
          as.double(maxval),
          as.integer(rep((1:n)-1,p)),
          as.double(x),
          PACKAGE="broman")$x

  z[z > maxval] <- NA
  matrix(z, ncol=p)
}

# normalize.R
