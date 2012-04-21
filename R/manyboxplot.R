######################################################################
# copyright (c) 2012, Karl W Broman
# First written Apr, 2012
# Last modified Apr, 2012
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
# Contains: manyboxplot
#
# a box-plot like figure, for *many* groups
#
# x is (n x p) where p is the number of groups
# probs is a vector with 0 <= probs < 1/2
#   - will always include dots at the median and make symmetric
######################################################################

manyboxplot <-
function(x, probs=c(0.05, 0.1, 0.25), dotcol="blue",
         linecol=c("black","red","green", "orange"),
         ...)
{
  if(!all(probs >= 0 & probs < 1/2))
    stop("probs should be >=0 and < 1/2")
  if(length(linecol) < length(probs))
    stop("length(probs) > length(linecol) ", length(probs), " > ", length(linecol))
  p <- ncol(x)
  if(p < 2) stop("ncol(x) should be >= 2")

  probs <- sort(probs)
  probs <- c(probs, 0.5, rev(1-probs))

  xqu <- apply(x, 2, quantile, probs, na.rm=TRUE)

  # this is to deal with varying inputs (did "..." include xaxs or not?)
  manyboxplot.sub <-
  function(xqu, dotcol, xlab="", xaxs="i", ylab="quantiles",
           xlim=c(0.25, ncol(xqu)+.75), linecol, type="p", 
           xat, lwd=2, lty=1, pch=16, ylim=range(xqu), ...)
  {
    medrow <- (nrow(xqu)-1)/2+1
    ncolx <- ncol(xqu)

    if(missing(xat)) {
      xat <- pretty(1:ncolx)
      xat <- c(1, xat[xat > 0])
    }

    grayplot(1:ncolx, xqu[medrow,], col=dotcol, xlab=xlab, type=type,
             xlim=xlim, xaxs=xaxs, pch=pch, ylim=ylim, ylab=ylab, xat=xat, ...)
    for(j in 1:(medrow-1)) {
      lines(1:ncolx, xqu[j,], col=linecol[j], lwd=lwd, lty=lty)
      lines(1:ncolx, xqu[nrow(xqu)-j+1,], col=linecol[j], lwd=lwd, lty=lty)
    }
  }

  manyboxplot.sub(xqu, dotcol=dotcol, linecol=linecol, ...)
  invisible()
}


