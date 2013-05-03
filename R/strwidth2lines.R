
strwidth2lines <- function(s, ...){
  (max(strwidth(s, units="inch", ...))/
    par("cin")[2]+par("mgp")[2])*par("cex")
}

