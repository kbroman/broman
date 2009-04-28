######################################################################
#
# mypairs.R
#
# copyright (c) 2006, Karl W Broman
# First written Jun, 2006
# Licensed under the GNU General Public License version 2 (June, 1991)
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
