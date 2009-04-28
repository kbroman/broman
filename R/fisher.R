######################################################################
#
# fisher.R
#
# copyright (c) 2001-8, Karl W Broman
# Nov, 2001; Apr, 2002; Dec, 2007; Jan, 2008
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
# Contains: fisher
#
######################################################################

######################################################################
#
# fisher: "approximate" fisher's exact test
#
######################################################################

fisher <-
function(tab, n.sim=1000)
{
  bot0 <- sum(lgamma(tab+1))
  bot <- 1:n.sim
  a <- list(rep(row(tab),tab),rep(col(tab),tab))
  for(i in 1:n.sim) {
    a[[1]] <- sample(a[[1]])
    tab2 <- table(a)
    bot[i] <- sum(lgamma(tab2+1))
  }
  mean(bot0 <= bot)
}

######################################################################
# chisq: approximate chi-square test
######################################################################
chisq <-
function(tab, n.sim=1000)
{
  observed <- suppressWarnings(chisq.test(tab)$stat)
  sims <- 1:n.sim
  a <- list(rep(row(tab),tab),rep(col(tab),tab))
  for(i in 1:n.sim) {
    a[[1]] <- sample(a[[1]])
    tab2 <- table(a)
    sims[i] <- suppressWarnings(chisq.test(tab2)$stat)
  }
  mean(sims >= observed)
}


# end of fisher.R
