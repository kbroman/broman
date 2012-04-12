######################################################################
#
# myround.R
#
# copyright (c) 2002-2012, Karl W Broman
# First written Aug, 2002
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
# Contains: myround
#
######################################################################

myround <-
function(x, digits=1)
{
  if(digits < 1) 
    stop("This is intended for the case digits >= 1.")
  
  if(length(digits) > 1) {
    digits <- digits[1]
    warning("Using only digits[1]")
  }

  sprintf(paste("%.", digits, "f", sep=""), x)
}
 
# end of myround.R
