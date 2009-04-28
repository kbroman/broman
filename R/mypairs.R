######################################################################
#
# mypairs.R
#
# copyright (c) 2006, Karl W Broman
# First written Jun, 2006
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
# Contains: mypairs
#
######################################################################

######################################################################
#
# mypairs: Like the pairs() function, but doing just the upper
#          triangle and taking par() arguments in the "..."
#
######################################################################

mypairs <-
function(x, ...)
{
  n <- ncol(x)
  if(is.null(colnames(x)))
    nam <- 1:n
  else
    nam <- colnames(x)

  z <- matrix(n^2, n-1, n-1, byrow=TRUE)
  m <- choose(n,2)
  z[!upper.tri(t(z))] <- 1:m
  z <- t(z)
  z[z>m] <- m + 1:sum(z>m)

  layout(z)

  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      plot(x[,j], x[,i], xlab=nam[j], ylab=nam[i], ...)
    }
  }
}

# end of mypairs.R
