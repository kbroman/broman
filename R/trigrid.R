#' Add grid lines to triplot
#'
#' Add grid lines to a ternary plot with [triplot()]
#'
#' @param n Number of grid lines
#' @param col Color of grid lines
#' @param lty Line type for grid lines
#' @param lwd Line width of grid lines
#' @param outer_col Color of outer triangle (If NULL, not plotted)
#' @param outer_lwd Line width of outer triangle
#' @param ... Additional arguments passed to [trilines()]
#'
#' @seealso [triplot()], [trilines()]
#'
#' @export
#'
#' @examples
#' triplot(c("A","H","B"), gridlines=1, grid_lwd=2)
#' trigrid(3, lty=2, lwd=2)
trigrid <-
    function(n=1, col="white", lty=1, lwd=1,
             outer_col="black", outer_lwd=2,
             ...)
{
    stopifnot(n>=1)
    if(abs(n-round(n)) > 1e-6) {
        warning("n should be a positive integer")
        n <- round(n)
    }

    gr <- seq(1, n)/(n+1)
    p1 <- cbind(gr, 0, 1-gr)
    p2 <- cbind(gr, 1-gr, 0)
    p3 <- cbind(0, 1-gr, gr)
    p4 <- cbind(1-gr, 0, gr)

    for(i in 1:nrow(p1)) {
        trilines(rbind(p1[i,], p2[i,]), lty=lty, lwd=lwd, col=col, ...)
        trilines(rbind(p2[i,], p3[i,]), lty=lty, lwd=lwd, col=col, ...)
        trilines(rbind(p3[i,], p4[i,]), lty=lty, lwd=lwd, col=col, ...)
    }

    if(!is.null(outer_col)) { # plot outer triangle again
        trilines(rbind(c(1,0,0), c(0,1,0), c(0,0,1), c(1,0,0)),
                 col=outer_col, lwd=outer_lwd)
    }

}
