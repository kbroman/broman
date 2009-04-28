######################################################################
#
# normalize.R
#
# copyright (c) 2005, Karl W Broman
# last modified 28 Oct 2005
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
#  force two sets of intensities to have the same marginal distributions
######################################################################
normalize <-
function(x,y)
{
  if(!missing(y)) x <- cbind(x,y)
  if(is.data.frame(x)) x <- as.matrix(x)

  n <- nrow(x)
  p <- ncol(x)

  z <- .C("R_normalize",
          as.integer(n),
          as.integer(p),
          x=as.double(x),
          as.integer(rep(1:n-1,p)),
          as.double(x),
          PACKAGE="broman")

  matrix(z$x,ncol=p)
}

# normalize.R
