######################################################################
#
# quantileSE.R
#
# copyright (c) 2001, Karl W Broman
# Nov, 2001
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
# Contains: quantileSE
#
######################################################################

######################################################################
#
# quantileSE: calculate quantiles plus approximate SEs
#
######################################################################

quantileSE <-
function(x, p=0.95, bw, na.rm=TRUE, names=TRUE)
{
  if(na.rm) x <- x[!is.na(x)]
  quant <- quantile(x,p)
  R <- sqrt(p*(1-p)/length(x))
  if(missing(bw)) 
    f <- sapply(quant, function(a,b) density(b,from=a,to=a,n=1)$y,x)
  else
    f <- sapply(quant, function(a,b) density(b,bw=bw,from=a,to=a,n=1)$y,x)

  out <- rbind(quantile=quant,SE=R/f)
  if(names) colnames(out) <- as.character(p)
  out
}

# end of quantileSE.R
