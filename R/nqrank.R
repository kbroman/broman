######################################################################
#
# nqrank.R
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
# Contains: nqrank
#
######################################################################

######################################################################
#
# nqrank: Convert a set of quantitative values to the corresponding
#         normal quantiles
#
######################################################################

nqrank <-
function(x)
{
  y <- x[!is.na(x)]
  y[y == Inf] <- max(y[y<Inf])+10
  y[y == -Inf] <- min(y[y > -Inf]) + 10
  y <- rank(y+runif(length(y))/(sd(y)*10^8))
  x[!is.na(x)] <- qnorm((y-0.5)/length(y))
  x
}

# end of nqrank.R
