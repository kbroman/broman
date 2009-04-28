######################################################################
#
# qr.R
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
# Contains: qr2
#
######################################################################

######################################################################
#
# qr2: Pulls out Q and R for Q-R decomposition given by qr()
#
######################################################################

qr2 <-
function(x, tol=1e-7)
{
  qq <- qr(x, tol=tol)
  p <- ncol(x); n <- nrow(x)

  r0 <- matrix(0,p,p)
  r0[row(r0) <= col(r0)] <-
    qq$qr[row(qq$qr) <= col(qq$qr)]
  r0 <- sweep(r0,1,(-1)^(1:p),"*")

  q0 <- qr.qy(qq,diag(1,n)[,1:p])
  q0 <- sweep(q0,2,(-1)^(1:p),"*")

  list(q=q0,r=r0)
}

# end of qr.R
