######################################################################
# runningmean.R
#
# Karl W Broman
# last modified Sep, 2007
# first written Sep, 2005
#
# Contains: runningmean, runningratio
######################################################################

######################################################################
# get running mean, sum or median within a specified window
######################################################################
runningmean <-
function(pos, value, window=1000, what=c("mean","sum", "median"))
{
  what <- which(c("sum","mean","median")==match.arg(what))
  
  n <- length(pos)
  if(length(value) != n)
    stop("pos and value must have the same length\n")

  .C("R_runningmean",
     as.integer(n),
     as.double(pos),
     as.double(value),
     z=as.double(rep(0,n)),
     as.double(window),
     as.integer(what),
     PACKAGE="broman")$z
}


######################################################################
# runningratio
#
# take sum(numerator)/sum(denominator) in sliding window
######################################################################
runningratio <-
function(pos, numerator, denominator, window=1000)
{
  n <- length(pos)
  if(length(numerator) != n || length(denominator) != n)
    stop("pos, numerator and denominator must all be the same length\n")

  .C("R_runningratio",
     as.integer(n),
     as.double(pos),
     as.double(numerator),
     as.double(denominator),
     z=as.double(rep(0,n)),
     as.double(window),
     PACKAGE="broman")$z
}

# end of runningmean.R
