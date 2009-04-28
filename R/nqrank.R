######################################################################
#
# nqrank.R
#
# copyright (c) 2001, Karl W Broman
# Nov, 2001
# Licensed under the GNU General Public License version 2 (June, 1991)
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
