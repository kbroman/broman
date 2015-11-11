#  arrowlocator
#'
#' Use the locator function to plot an arrow
#'
#' Use the \code{\link[graphics]{locator}} function to indicate the
#'   endpoints of an arrow and then plot it.
#'
#' @param reverse If FALSE, first indicate the tail of the arrow and
#'     then the head; if TRUE, first indicate the head of the arrow and then
#'     the tail.
#'
#' @param horizontal If TRUE, force the arrow to be horizontal.  (Use the
#'   average y-axis value of the two clicks for the vertical placement.)
#'
#' @param vertical If TRUE, force the arrow to be vertical.  (Use the
#'   average x-axis value of the two clicks for the horizontal placement.)
#'
#' @param length Length of the edges of the arrow head.
#'
#' @param ... Additional graphics parameters
#'
#' @details
#' Use \code{\link[graphics]{locator}} to indicate the two endpoints of
#'   an arrow and then draw it.
#'
#' @importFrom graphics locator arrows
#' @export
#' @return
#' The locations of the endpoints of the arrow, as a two-row
#'   matrix.  The first row indicates the location of the tail of the
#'   arrow; the second row indicates the location of the head of the
#'   arrow.
#'
#' @examples
#' \dontrun{
#' plot(0,0,type="n", xlab="", ylab="", xlim=c(0,100), ylim=c(0,100))
#' arrowlocator(col="blue", lwd=2)
#' }
#'
#' @seealso
#' \code{\link[graphics]{arrows}},
#'   \code{\link[graphics]{locator}}
#'
#' @keywords
#' hplot
arrowlocator <-
    function(reverse=FALSE, horizontal=FALSE, vertical=FALSE, length=0.1, ...)
{
    x <- locator(2)
    if(reverse) x <- lapply(x, rev)
    if(horizontal) x[[2]] <- rep(mean(x[[2]]), 2)
    if(vertical) x[[1]] <- rep(mean(x[[1]]), 2)

    arrows(x[[1]][1], x[[2]][1], x[[1]][2], x[[2]][2], length=length, ...)

    x <- matrix(unlist(x), ncol=2)
    dimnames(x) <- list(c("tail","head"), c("x","y"))
    invisible(x)
}
