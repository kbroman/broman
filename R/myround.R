######################################################################
#
# myround.R
#
# copyright (c) 2006, Karl W Broman
# Aug, 2002
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
  
  y <- as.character(round(x, digits))

  z <- strsplit(y, "\\.")
  sapply(z, function(a, digits)
         {
           if(length(a) == 1)
             b <- paste(a[1], ".", paste(rep("0", digits),collapse=""), sep="")
           else {
             if(nchar(a[2]) == digits)
               b <- paste(a, collapse=".")
             else
               b <- paste(a[1], ".", a[2],
                          paste(rep("0", digits - nchar(a[2])), collapse=""),
                          sep="")
           }
         }, digits)
}
         
 
# end of myround.R
