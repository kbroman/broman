######################################################################
#
# rmvn.R
#
# copyright (c) 2001, Karl W Broman
# Nov, 2001
# Licensed under the GNU General Public License version 2 (June, 1991)
# 
# Part of the R/broman package
# Contains: rmvn
#
######################################################################

######################################################################
#
# rmvn: simulate from multivariate normal distribution
#
######################################################################

rmvn <-
function(n, mu=0, V=matrix(1))
{
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  matrix(rnorm(n*p),ncol=p) %*% D + rep(mu,rep(n,p))
}

# end of rmvn.R
