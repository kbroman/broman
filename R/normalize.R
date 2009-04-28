######################################################################
#
# normalize.R
#
# copyright (c) 2005, Karl W Broman

# Licensed under the GNU General Public License version 2 (June, 1991)
# last modified 28 Oct 2005
# first written 18 Sep 2005
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
