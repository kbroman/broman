######################################################################
#
# simp.R: Numerical integration by Simpson's rule
#
# copyright (c) 2001, Karl W Broman
# Nov, 2001
# Licensed under the GNU General Public License version 2 (June, 1991)
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
