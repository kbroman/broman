######################################################################
#
# holmans_triangle.R
#
# copyright (c) 2006-2010, Karl W Broman
# Last modified Nov, 2010
# First written May, 2006
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
# Contains: triplot, tripoints, trilines, triarrow
#
######################################################################

triplot <-
function(labels, ...)
{
  m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

  pts <- m %*% diag(rep(1,3))
  lim <- apply(pts,1,range)
  rlim <- apply(lim,2,diff)
  lim[1,] <- lim[1,] - rlim*0.15
  lim[2,] <- lim[2,] + rlim*0.15

  plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
       xlim=lim[,1], ylim=lim[,2], xaxs="i", yaxs="i", ...)

  pin <- par("pin")
  if(pin[2] > pin[1]) 
    pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
  else 
    pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2

  for(i in 1:2) pts[i,] <- pts[i,] - mean(range(pts[i,])) + mean(range(lim[,i]))

  if(missing(labels))
    labels <- c("(1,0,0)", "(0,1,0)", "(0,0,1)")

  ya <- c(0,rlim[2]*0.06,0)
  xa <- c(rlim[1],0,-rlim[1])*0.06
  for(i in 1:3)
    text(pts[1,i]+xa[i], pts[2,i]+ya[i], labels[i])
  for(i in 1:2) {
    for(j in (i+1):3)
      segments(pts[1,i], pts[2,i], pts[1,j], pts[2,j], lwd=2)
  }
}
  

tripoints <-
function(x, ...)
{
  m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

  pts <- m %*% diag(rep(1,3))
  lim <- apply(pts,1,range)
  rlim <- apply(lim,2,diff)
  lim[1,] <- lim[1,] - rlim*0.12
  lim[2,] <- lim[2,] + rlim*0.12

  x <- as.matrix(x)
  if(is.matrix(x) && nrow(x) != 3) x <- t(x)
  if(any(abs(colSums(x) - 1) > 1e-6)) {
    x <- x / colSums(x)
    warning("Some columns do not sum to 1; rescaling.")
  }

  x <- t(m %*% x)
  pin <- par("pin")
  if(pin[2] > pin[1]) {
    pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
    x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
  }
  else {
    x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
    pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
  }
  for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

  points(x, ...)
}

trilines <-
function(x, ...)
{
  m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

  pts <- m %*% diag(rep(1,3))
  lim <- apply(pts,1,range)
  rlim <- apply(lim,2,diff)
  lim[1,] <- lim[1,] - rlim*0.12
  lim[2,] <- lim[2,] + rlim*0.12

  x <- as.matrix(x)
  if(is.matrix(x) && nrow(x) != 3) x <- t(x)
  if(any(abs(colSums(x) - 1) > 1e-6)) {
    x <- x / colSums(x)
    warning("Some columns do not sum to 1; rescaling.")
  }

  x <- t(m %*% x)
  pin <- par("pin")
  if(pin[2] > pin[1]) {
    pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
    x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
  }
  else {
    x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
    pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
  }
  for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

  lines(x, ...)
}

triarrow <-
function(x, ...)
{
  if(nrow(x) == 2 && ncol(x) == 3) 
    x <- t(x)
  else if(!(nrow(x)==3 && ncol(x)==2))
    stop("x must be a 2x3 or 3x2 matrix")
  
  m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

  pts <- m %*% diag(rep(1,3))
  lim <- apply(pts,1,range)
  rlim <- apply(lim,2,diff)
  lim[1,] <- lim[1,] - rlim*0.12
  lim[2,] <- lim[2,] + rlim*0.12

  x <- as.matrix(x)
  if(any(abs(colSums(x) - 1) > 1e-6)) {
    x <- x / colSums(x)
    warning("Some columns do not sum to 1; rescaling.")
  }

  x <- t(m %*% x)
  pin <- par("pin")
  if(pin[2] > pin[1]) {
    pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
    x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
  }
  else {
    x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
    pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
  }
  for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

  arrows(x[1,1], x[1,2], x[2,1], x[2,2], ...)
}

# end of holmans_triangle.R
