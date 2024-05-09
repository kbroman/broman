#' Scatterplot with date/times on the x-axis
#'
#' Like the [grayplot()] function, but with the x-axis having date/times
#'
#' @param x X-axis coordinates of points for the plot (must be date/time values)
#'
#' @param y Y-axis coordinates of points for the plot
#'
#' @param ... Optional graphics arguments passed to [grayplot()]
#'
#' @param n Approximate number of x-axis labels (passed to [base::pretty()]).
#'
#' @param scale Passed to [time_axis()] for defining the x-axis labels
#'
#' @param format Passed to [time_axis()] for defining the x-axis labels
#'
#' @return None.
#'
#' @export
#'
#' @importFrom graphics axis
#'
#' @seealso [time_axis()], [grayplot()], [dotplot()]
#'
#' @examples
#' n <- 100
#' y <- rnorm(n)
#' x <- seq(as.POSIXct("2024-05-01 11:23"), as.POSIXct("2024-05-01 14:50"), length.out=n)
#' timeplot(x, y)

timeplot <-
    function(x, y, ..., n=5, scale=NULL, format=NULL)
{
    xax <- time_axis(x, n, scale, format)

    timeplot_internal <-
        function(x, y, vlines=NULL, mgp=c(2, 0.5, 0), mgp.x=NULL, mgp.y=NULL, ...)
    {
        if(is.null(vlines)) vlines <- xax$x

        if(is.null(mgp.x)) mgp.x <- mgp
        if(is.null(mgp.y)) mgp.y <- mgp

        grayplot(x, y, xat=NA, vlines=vlines, mgp=mgp, mgp.x=mgp.x, mgp.y=mgp.y, ...)

        axis(side=1, at=xax$x, labels=xax$labels, tick=FALSE, mgp=mgp.x)
    }

    timeplot_internal(x, y, ...)
    invisible()
}
