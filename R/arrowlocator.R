######################################################################
#
# arrowlocator.R
#
# copyright (c) 2010, Karl W Broman
# last modified: Nov, 2010
# first written: Nov, 2010
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
# Contains: arrowlocator
#
######################################################################

######################################################################
#
# arrowlocator: use locator() to draw an error; returning the
#               two endpoints
#
#   If reverse=TRUE, start with head; if reverse=FALSE, start with tail
#
######################################################################

arrowlocator <-
function(reverse=FALSE, horizontal=FALSE, vertical=FALSE, length=0.1, ...)
{
  x <- locator(2)
  if(reverse) x <- lapply(x, rev)
  if(horizontal) x[[2]] <- rep(mean(x[[2]]), 2)
  if(vertical) x[[1]] <- rep(mean(x[[1]]), 2)
  
  arrows(x[[1]][1], x[[2]][1], x[[1]][2], x[[2]][2], length=length, ...)

  x <- matrix(unlist(x), ncol=2)
  dimnames(x) <- list(c("tail","head"), c("x","y"))
  invisible(x)
}

# end of arrowlocator.R
