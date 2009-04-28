######################################################################
# convert a number to hex notation
######################################################################

convert2hex <-
function(d)
{
  hex <- c(0:9,LETTERS[1:6])
  first <- d %/% 16
  second <- d %% 16
  paste(hex[first+1],hex[second+1],sep="")
}

  
# end of convert2hex.R
