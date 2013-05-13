######################################################################
#
# histlines.R
#
# copyright (c) 2013, Karl W Broman
# First written May, 2013
# Last modified May, 2013
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
# Contains: histlines
#
######################################################################

# Either:
# x = breaks for histogram; y = counts or density
#
# or:
# x = data, breaks=pass to hist()
histlines <-
function(x, y, breaks, use=c("counts", "density")) 
{
  if(missing(y)) { # input doesn't count the count information
    out <- hist(x, breaks=breaks, plot=FALSE)
    x <- out$breaks
    use <- match.arg(use)
    y <- out[[use]]
  }

  if(length(x) != length(y)+1)
    stop("length(x) != length(y) + 1")

  x <- as.numeric(rbind(x, x))
  y <- c(0, as.numeric(rbind(y,y)), 0)
  data.frame(x=x, y=y)
}

