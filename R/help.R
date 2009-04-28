######################################################################
#
# help.R
#
# copyright (c) 2007, Karl W Broman
# Jan, 2007
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
# Contains: h
#
######################################################################

######################################################################
#
# h: get html version of help within ess
#
######################################################################

h <-
function(...)
{
  help(..., htmlhelp=TRUE)
  
}

# end of help.R

