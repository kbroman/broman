######################################################################
#
# quantileSE.R
#
# copyright (c) 2001, Karl W Broman
# Nov, 2001
# Licensed under the GNU General Public License version 2 (June, 1991)
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
