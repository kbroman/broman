####################################################################
# venn_diagram.R
#
# copyright (c) 2003-2010, Karl W Broman
# Last modified Nov, 2010
# First written May, 2003
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
# Contains: venn 
#
# Draw a to-scale Venn diagram:
#     Two circles whose area is prop'l to the number of data points
#     in two sets, and for which the area of the region of overlap is
#     proportional to the number of data points in both sets.
#####################################################################

venn <-
function(setA=50, setB=50, both=25,
         method=c("circle", "square"), labels=c("A","B"),
         col=c("blue","red"))
{
  if(setA < 0 || setB < 0 || both < 0)
    stop("The arguments must by non-negative.\n")
  if(both > setA || both > setB)
    stop("both must be < each of setA, setB.\n")
  setAonly <- setA-both
  setBonly <- setB-both

  method <- match.arg(method)

  if(method=="square") {
    # 1/2 lengths of the sides
    rA <- sqrt(setAonly + both)/2
    rB <- sqrt(setBonly + both)/2

    # y-axis location of centers of circles
    yA <- yB <- 0

    # x-axis location of centers of circles
    xA <- 0
    if(both==0) { # no overlap
      xB <- rA+rB+min(c(rA,rB))/4
    }
    else {
      if(setAonly == 0 || setBonly == 0) {
        xB <- 0
      }
      else {
        xB <- rA + rB - both/min(c(rA,rB))/2
      }
    }

    # center at (0,0)
    ctr <- (min(c(xA-rA,xB-rB)) + max(c(xA+rA,xB+rB)))/2
    xA <- xA - ctr
    xB <- xB - ctr

    ctr <- (min(c(yA-rA,yB-rB)) + max(c(yA+rA,yB+rB)))/2
    yA <- yA - ctr
    yB <- yB - ctr

    # x- and y-axis limits
    xli <- c(xA-rA,xB+rB)
    yli <- c(-1,1)*max(c(rA,rB))
    xli <- yli <- c(min(c(xli[1],yli[1])),
                    max(c(xli[2],yli[2])))

    # create empty plot figure
    par(pty="s",bty="n",mar=c(0.1,0.1,0.1,0.1))
    plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=xli, ylim=yli)

    # plot the rectangles
    rect(xA-rA,yA-rA,xA+rA,yA+rA,lwd=2,border=col[1],angle=0)
    rect(xB-rB,yB-rB,xB+rB,yB+rB,lwd=2,border=col[2],angle=0)

    if(!is.null(labels)) {
      gap <- ((xB+rB)-xA)*0.02
      text(xA-rA+gap,yA,labels[1],adj=c(0,0), col=col[1])
      text(xB+rB-gap,yB,labels[2],adj=c(1,0),col=col[2])
    }
  }
  else {
  

    # radiuses of the circles
    rA <- sqrt((setAonly + both)/pi)
    rB <- sqrt((setBonly + both)/pi)

    # y-axis location of centers of circles
    yA <- yB <- 0

    ##############################
    # the key subroutine
    ##############################
    # find distance between circle centers to give a particular area
    #     we assume here that rB >= rA
    find.distance <-
    function(rA,rB,area)
    {
      # find area of overlap for two circles given radiuses
      #     and given distance betwen their centers
      find.overlap <-
      function(rA,rB,d.betw.ctrs)
      {
        if(d.betw.ctrs == rB-rA) return(pi*rA^2)
        if(d.betw.ctrs == rB+rA) return(0)
 
        x <- (d.betw.ctrs^2 +rA^2 - rB^2)/(2*d.betw.ctrs)
        y <- sqrt(rA^2 - x^2)

        if(x >= 0)
          return(asin(y/rA)*rA^2 - y*x +
                 asin(y/rB)*rB^2 - y*(d.betw.ctrs-x))
        else
         return(rA^2*pi - asin(y/rA)*rA^2 - y*x +
                asin(y/rB)*rB^2 - y*(d.betw.ctrs-x))
      }

      g <- function(d,rA,rB,area) find.overlap(rA,rB,d)-area
      uniroot(g,lower=rB-rA,upper=rB+rA, rA=rA, rB=rB,area=area)$root
    }
    ##############################
    # back to the venn() function
    ##############################

    # x-axis location of centers of circles
    xA <- 0
    if(both==0) { # no overlap
      xB <- rA+rB+min(c(rA,rB))/4
    }
    else {
      if(setAonly == 0 || setBonly == 0) {
        xB <- abs(rB-rA)/2
      }
      else {
        xB <- find.distance(min(c(rA,rB)),max(c(rA,rB)),both)
      }
    }

    # x- and y-axis limits
    xli <- yli <- c(min(c(xA-rA,yA-rA,xB-rB,yB-rB)),
                    max(c(xA+rA,yA+rA,xB+rB,yB+rB)))

    # adjust the centers to make picture symmetric
    yB <- yA <- mean(yli)

    left <- min(c(xA-rA,xB-rB)) - xli[1]
    right <- xli[2] - max(c(xA+rA,xB+rB))
    shift <- (left+right)/2 - left
    xA <- xA + shift
    xB <- xB + shift

    # create empty plot figure
    par(pty="s",bty="n",mar=c(0.1,0.1,0.1,0.1))
    plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=xli, ylim=yli)

    # plot the circles
    z <- seq(0,2*pi,length=201)
    lines(rA*cos(z)+xA,rA*sin(z)+yA,lwd=2,col=col[1])
    lines(rB*cos(z)+xB,rB*sin(z)+yB,lwd=2,col=col[2])

    if(!is.null(labels)) {
      gap <- ((xB+rB)-xA)*0.02
      text(xA-rA+gap,yA,labels[1],adj=c(0,0),col=col[1])
      text(xB+rB-gap,yB,labels[2],adj=c(1,0),col=col[2])
    }
  }
  return(invisible())
}


######################################################################
# venn3
#
# Function to draw a venn diagram with 3 groups, "to scale",
# using rectangles
######################################################################

# I think this isn't working right

#venn3 <-
#function(A=17, B=37, C=55,
#         AB=9, AC=12, BC=28,
#         ABC=9, labels=c("A","B","C"),
#         col=c("black","blue","red"))
#{
#  # make A < B < C
#  if(A > B) {
#    x <- B;  B <- A;  A <- x
#    x <- BC;  BC <- AC;  AC <- x
#  }
#  if(B > C) {
#    x <- C; C <- B; B <- x
#    x <- AC; AC <- AB; AB <- x
#  }
#
#
#  x3 <- BC/sqrt(B)
#  y3 <- ABC/x3
#  y2 <- BC/x3-y3
#  y4 <- (AB-ABC)/x3
#  x2 <- AC/(y3+y4) - x3
#  x1 <- sqrt(C)-x2-x3
#  x4 <- sqrt(A)-x2-x3
#  x5 <- sqrt(B)-x3-x4
#  y1 <- sqrt(C)-y2-y3-y4
#  y5 <- sqrt(A)-y3-y4
#  x <- cumsum(c(0,x1,x2,x3,x4,x5))
#  y <- cumsum(c(0,y1,y2,y3,y4,y5))
#
#
#  lim <- c(0, max(c(x,y)))
#
#  if(max(x) < max(y))
#    x <- x + (max(y)-max(x))/2
#  else
#    y <- y + (max(x)-max(y))/2
#
#  oldmar <- par("mar")
#  oldpty <- par("pty")
#  oldbty <- par("bty")
#  on.exit(par(mar=oldmar,bty=oldbty,pty=oldpty))
#  par(mar=rep(1.1,4),bty="n",pty="s")
#  plot(0,0,type="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=lim,ylim=lim)
#
#  rect(x[1],y[1],x[4],y[5],lwd=3, border=col[1])
#
#  rect(x[3],y[2],x[6],y[4],lwd=3,border=col[2])
#
#  rect(x[2],y[3],x[5],y[6],lwd=3,border=col[3])
#
#  gap <- (x[5]-x[1])*0.02
#  text(x[1]+gap,y[1]+gap,labels[3],adj=c(0,0),col=col[1])
#  text(x[6]-gap,y[2]+gap,labels[2],adj=c(1,0),col=col[2])
#  text(x[2]+gap,y[6]-gap,labels[1],adj=c(0,1),col=col[3])
#  invisible()
#}


# end of venn_diagram.R

