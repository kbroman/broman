######################################################################
#
# help.R
#
# copyright (c) 2007, Karl W Broman
# Jan, 2007
# Licensed under the GNU General Public License version 2 (June, 1991)
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

