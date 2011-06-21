######################################################################
#
# winsorize.R
#
# copyright (c) 2011, Karl W Broman
# last modified Apr, 2011
# first written Apr, 2011
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
# Contains: winsorize
#
######################################################################

######################################################################
# winsorize a vector
#
# (move values above and below the alpha and 1-alpha quantiles
# to those quantiles)
######################################################################
winsorize <- 
function(x, q=0.006)
{
  lohi <- quantile(x, c(q, 1-q), na.rm=TRUE)
  if(diff(lohi) < 0) lohi <- rev(lohi)
  
  x[!is.na(x) & x < lohi[1]] <- lohi[1]
  x[!is.na(x) & x > lohi[2]] <- lohi[2]
  x
}

# end of winsorize.R
