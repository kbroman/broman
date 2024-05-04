#' Set up a time-based axis
#'
#' Set up a time-based axis for base graphics
#'
#' @param times A vector of date/times that will be plotted
#'
#' @param n Number of values to use in axis
#'
#' @return A data frame with the numeric values to plot plus labels to use.
#'
#' @export

time_axis <-
    function(times, n=8)
{
    if(!("POSIXct" %in% class(times) || "POSIXt" %in% class(times))) {
        stop("times should be a vector of date/times")
    }

    prettyx <- pretty(times, n=n)

    r <- range(as.numeric(times))
    dr <- diff(r)
    # determine range
    if(dr < 70) { # seconds
        labels <- format(prettyx, format="%S")
    }
    else if(dr < 60*70) { # minutes
        labels <- format(prettyx, format="%H:%M")
    }
    else if(dr < 60*60*26) { # hours
        labels <- format(prettyx, format="%H:%M")
    }
    else { # days
        labels <- format(prettyx, format="%Y-%m-%d")
    }

    # return vector of "pretty" times + labels
    data.frame(x=prettyx, label=labels)
}
