#' Set up a time-based axis
#'
#' Set up a time-based axis for base graphics
#'
#' @param times A vector of date/times that will be plotted
#'
#' @param n Number of values to use in axis
#'
#' @param scale Forced choice of scale for axis labels:
#'    `"sec"`, `"min"`, `"hr"`, or `"day"`. If NULL, scale is chosen
#'    based on the `times`.
#'
#' @return A data frame with the numeric values to plot plus labels to use.
#'
#' @export
#'
#' @examples
#' n <- 100
#' y <- rnorm(n)
#'
#' # labels as days
#' x <- seq(as.POSIXct("2024-05-01 11:23"), as.POSIXct("2024-05-07 14:50"), length.out=n)
#' xax <- time_axis(x)
#' grayplot(x, y, xat=NA, vlines=xax$x)
#' axis(side=1, at=xax$x, labels=xax$label, mgp=c(2.1, 0.5, 0), tick=FALSE)
#'
#' # labels as HH:MM
#' x <- seq(as.POSIXct("2024-05-01 11:23"), as.POSIXct("2024-05-01 14:50"), length.out=n)
#' xax <- time_axis(x)
#' grayplot(x, y, xat=NA, vlines=xax$x)
#' axis(side=1, at=xax$x, labels=xax$label, mgp=c(2.1, 0.5, 0), tick=FALSE)
#'
#'
#' # labels as seconds
#' x <- seq(as.POSIXct("2024-05-01 11:23:05.3"), as.POSIXct("2024-05-01 11:23:55.7"), length.out=n)
#' xax <- time_axis(x)
#' grayplot(x, y, xat=NA, vlines=xax$x)
#' axis(side=1, at=xax$x, labels=xax$label, mgp=c(2.1, 0.5, 0), tick=FALSE)

time_axis <-
    function(times, n=8, scale=NULL)
{
    if(!("POSIXct" %in% class(times) || "POSIXt" %in% class(times))) {
        stop("times should be a vector of date/times")
    }

    prettyx <- pretty(times, n=n)

    r <- range(as.numeric(times))
    dr <- diff(r)
    # determine range
    if(!is.null(scale)) {
        scales <- c("sec", "min", "hr", "day")
        if(!(scale %in% scales)) {
            warning("scale ignored; should be one of ",
                    vec2string(scales, "or"))
            scale <- NULL
        }
    }
    if(is.null(scale)) {
        if(dr < 70) scale <- "sec"
        else if(dr < 60*70) scale <- "min"
        else if(dr < 60*60*55) scale <- "hr"
        else dr <- "day"
    }

    if(scale=="sec") { # seconds
        labels <- format(prettyx, format="%S")
    }
    else if(scale=="min" || scale=="hr") { # minutes or hours
        labels <- format(prettyx, format="%H:%M")
    }
    else { # days
        labels <- format(prettyx, format="%Y-%m-%d")
    }

    # return vector of "pretty" times + labels
    data.frame(x=prettyx, labels=labels)
}
