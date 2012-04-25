######################################################################
#
# cf.R
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
# Contains: cf
#
######################################################################

cf <- function(a, b) UseMethod("cf")

cf.default <-
function(a, b)
((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))

cf.list <-
function(a,b)
{
  for(i in seq(along=a)) a[[i]] <- cf(a[[i]], b[[i]])
  a
}

### examples
#x <- matrix(rnorm(1000), ncol=20)
#x[sample(seq(along=x), 100)] <- NA
#all(cf(x,x))
#dim(cf(x,x))

#y <- x
#y[4,8] <- NA
#sum(!cf(x,y))
#y[6,2] <- 18
#sum(!cf(x,y))
#y[6,5] <- 32
#sum(!cf(x,y))

#x <- as.data.frame(x)
#y <- as.data.frame(y)
#sum(!cf(x,y))

#x <- as.list(x)
#y <- as.list(y)
#sapply(cf(x,y), function(a) sum(!a))

# end of cf.R
