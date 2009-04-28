######################################################################
# convert a number to hex notation
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
