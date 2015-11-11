#  strwidth2xlim: Calculates xlim for adding labels to positions ##
#'
#' Calculate horizontal limit in user coordinates for adding labels
#'
#' Calculates the x-axis limits when adding (long) labels to a plot
#'
#' @param x numeric vector of horizontal coordinates
#'
#' @param xstring character vector, specifying text to be written
#'
#' @param pos position specifier for text; values of \code{1}, \code{2},
#'   \code{3}, and \code{4}, respectively, indicate positions below, to
#'   the left of, above, and to the right of the coordinates
#'
#' @param offset offset of the label from the coordinate in fractions of
#'     a character width
#'
#' @param ... additional text parameters from \code{par}, such as \code{cex}
#'
#' @details
#' See \code{text} for details on \code{pos} and \code{offset}.
#'
#' @importFrom graphics strwidth
#' @export
#' @return
#' Minimum and maximum x-axis limits for adding horizontal text
#'
#' @author
#' Aimee Teo Broman
#'
#' @examples
#' x <- runif(15,-1,1)*10
#' xlabs <- sapply(sample(1:20,15,replace=TRUE),
#'          function(a) paste(LETTERS[1:a], collapse=""))
#' ## Labels to the left ##
#' xlims <- strwidth2xlim(x,xlabs,pos=2)
#' plot(x,1:length(x),xlim=xlims)
#' text(x,1:length(x),xlabs,pos=2)
#' ## Labels to the right ##
#' xlims <- strwidth2xlim(x,xlabs,pos=4,cex=0.7)
#' plot(x,1:length(x),xlim=xlims)
#' text(x,1:length(x),xlabs,pos=4,cex=0.7)
#'
#' @seealso
#' \code{text}
strwidth2xlim <- function(x,xstring, pos=4, offset=0.5,...){
    xwid <- (strwidth(xstring,units="inches", ...)+
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
