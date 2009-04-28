######################################################################
#
# simp.R: Numerical integration by Simpson's rule
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
# Contains: subtrap, trap, simp
#
######################################################################

subtrap <-
function(f, a, b, n=1, ...)
{
  h <- (b-a)/n
  if(n==1) return( h*mean( f(c(a,b), ...)))
  else return(h*sum( f(seq(a+h,b,by=2*h),...) ))
}

trap <-
function(f, a, b, tol=1e-8, max.step=1000, ...)
{
  i.old <- subtrap(f,a,b,1,...); n <- 2
  for(i in 2:max.step) {
    s <- subtrap(f, a, b, n, ...)
    i.new <- i.old/2 + s
    if(abs(i.new-i.old) < tol) break
    i.old <- i.new
    n <- n*2
  }
  i.new
}

simp <-
function(f, a, b, tol=1e-8, max.step=1000, ...)
{
  i.old <- subtrap(f, a, b, 1, ...)*2/3
  n <- 2; old.s <- 0
  for(i in 2:max.step) {
    s <- subtrap(f, a, b, n, ...)
    i.new <- (i.old/2 + (4*s-old.s)/3)
    if(abs(i.new-i.old) < tol) break
    i.old <- i.new; old.s <- s
    n <- n*2
  }
  i.new
}

# end of simp.R
