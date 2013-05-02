## Calculates xlim for adding labels to positions ##

xlimlabel <- function(x,xlabels, pos=4, offset=0.5,...){
  dots <- list(...)
  cex <- if(!is.na(match("cex",names(dots)))) dots$cex else 1
  xwid <- (strwidth(xlabels,units="inches",cex=cex)+
           offset*par("cin")[1])/par("pin")[1]
  xmax <- max(x,na.rm=TRUE)
  xmin <- min(x,na.rm=TRUE)
  x1 <- x-xmax
  x2 <- x-xmin
  if(pos==2) return(c(min(x1/(1-xwid)),0)+xmax)
  if(pos==4) return(c(0, max(x2/(1-xwid)))+xmin)
  if(pos %in% c(1,3))
       return(c(min(x1/(1-0.5*xwid))+xmax,max(x2/(1-0.5*xwid))+xmin))
}
