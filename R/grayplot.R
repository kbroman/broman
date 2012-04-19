######################################################################
#
# grayplot.R
#
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
# Contains: grayplot
#
######################################################################

grayplot <-
function(x, y, ..., type="p", hlines, hlines.col="white", hlines.lty=1, hlines.lwd=1,
         vlines, vlines.col="white", vlines.lty=1, vlines.lwd=1,
         xat, yat, bgcolor="gray80")
{
  if(missing(hlines)) hlines <- NULL
  if(missing(hlines.col)) hlines.col <- "white"
  if(missing(vlines)) vlines <- NULL
  if(missing(vlines.col)) vlines.col
  if(missing(xat)) xat <- NULL
  if(missing(yat)) yat <- NULL
  if(missing(x)) stop("x unspecified")
  if(missing(y)) y <- NULL

  hidegrayplot <-
  function(x, y, ..., type="p", hlines, hlines.col, hlines.lty, hlines.lwd,
           vlines, vlines.col, vlines.lty, vlines.lwd,
           xat, yat, bgcolor="gray80", xaxt="n", yaxt="n",
           las=1)
  {
    if(is.null(y))
      plot(x, ..., type="n", xaxt="n", yaxt="n")
    else
      plot(x, y, ..., type="n", xaxt="n", yaxt="n")
      
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col=bgcolor, border="black")

    if(!is.null(xat)) {
      if(!is.null(vlines))       
        axis(side=1, at=xat, mgp=c(3, 0.5, 0), tick=FALSE, las=las)
      else
        axis(side=1, at=xat, las=las)
    }
    else {
      if(!is.null(vlines))
        axis(side=1, mgp=c(3,0.5,0), tick=FALSE, las=las)
      else
        axis(side=1, las=las)
    }

    if(!is.null(yat)) {
      if(!is.null(hlines))       
        axis(side=2, at=yat, mgp=c(3, 0.5, 0), tick=FALSE, las=las)
      else
        axis(side=2, at=yat, las=las)
    }
    else {
      if(!is.null(hlines))
        axis(side=2, mgp=c(3,0.5,0), tick=FALSE, las=las)
      else
        axis(side=2, las=las)
    }

    if(!is.null(hlines)) abline(h=hlines, col=hlines.col, lty=hlines.lty, lwd=hlines.lwd)
    if(!is.null(vlines)) abline(v=vlines, col=vlines.col, lty=vlines.lty, lwd=vlines.lwd)

    if(is.null(y)) points(x, ..., type=type)
    else points(x, y, ..., type=type)
  }

  hidegrayplot(x=x, y=y, ..., type=type, hlines=hlines, hlines.col=hlines.col,
               hlines.lty=hlines.lty, hlines.lwd=hlines.lwd,
               vlines=vlines, vlines.col=vlines.col,
               vlines.lty=vlines.lty, vlines.lwd=vlines.lwd,
               xat=xat, yat=yat, bgcolor=bgcolor)
  invisible()
}

# end of grayplot.R
