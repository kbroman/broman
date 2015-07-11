## Calculates xlim for adding labels to positions ##

#  xlimlabel
#'
#' Calulate horizontal limit in user coordinates for adding labels
#'
#' Calculates the x-axis limits when adding (long) labels to a plot
#'
#' @param x numeric vector of horizontal coordinates
#'
#' @param xlabels character vector, specifying text to be written
#'
#' @param pos position specifier for text; values of \code{1}, \code{2},
#'  \code{3}, and \code{4}, respectively, indicate positions below, to
#'  the left of, above, and to the right of the coordinates
#'
#' @param offset  offset of the label from the coordinate in fractions of
#'     a character width
#'
#' @param ... Additional par arguments
#'
#' @details
#' See \code{text} for details on \code{pos} and \code{offset}.
#'
#' @export
#' @importFrom graphics strwidth
#'
#' @return
#' Minimum and maximum x-axis limits for adding horizontal text
#'
#' @author
#' Aimee Teo Broman
#'
#' @examples
#' x <- runif(15, -1, 1)*10
#' xlabs <- sapply(sample(1:20, 15, replace=TRUE),
#'                 function(a) paste(LETTERS[1:a], collapse=""))
#' par(mfrow=c(2,1), las=1)
#' ## Labels to the left ##
#' xlims <- xlimlabel(x, xlabs, pos=2)
#' plot(x, 1:length(x), xlim=xlims, ylab="Index")
#' text(x, 1:length(x), xlabs, pos=2)
#' ## Labels to the right ##
#' xlims <- xlimlabel(x, xlabs, pos=4, cex=0.7)
#' plot(x, 1:length(x), xlim=xlims, ylab="Index")
#' text(x, 1:length(x), xlabs, pos=4, cex=0.7)
#'
#' @seealso
#' \code{\link[graphics]{text}}
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
