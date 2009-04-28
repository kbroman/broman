######################################################################
#
# myround.R
#
# copyright (c) 2006, Karl W Broman
# Aug, 2002
# Licensed under the GNU General Public License version 2 (June, 1991)
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
