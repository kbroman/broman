######################################################################
#
# util.R
#
# copyright (c) 2011-2012, Karl W Broman
# last modified Mar, 2012
# first written Apr, 2011
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
# Contains: bromanversion
#
######################################################################

######################################################################
# print the installed version of R/broman
######################################################################
bromanversion <-
function()
{
  version <- unlist(packageVersion("broman"))

  # make it like #.#-#
  paste(c(version,".","-")[c(1,4,2,5,3)], collapse="")
}

# end of util.R
