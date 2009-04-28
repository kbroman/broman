######################################################################
#
# fisher.R
#
# copyright (c) 2001-8, Karl W Broman
# Nov, 2001; Apr, 2002; Dec, 2007; Jan, 2008
# Licensed under the GNU General Public License version 2 (June, 1991)
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
