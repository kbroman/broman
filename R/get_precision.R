#' Determine the precision of a number
#'
#' Determine the precision of a number, as the number of digits past
#' the decimal point.
#'
#' @md
#'
#' @param x A numeric vector
#' @param ... Ignore this
#'
#' @return A vector of integers, with the number of digits (to the last non-zero digit) past the decimal point.
#'
#' @details If the number is expressed in scientific notation, we take the number of digits

#' @importFrom stats setNames
#' @export
get_precision <-
    function(x, ...)
{
    # a bit of contortion here to control the scipen and digits options and have them returned to their initial values
    dots <- list("...")
    if(is.null(dots$set_digits) || dots$set_digits) {
        scipen <- options("scipen")$scipen
        digits <- options("digits")$digits
        on.exit(options(scipen=scipen, digits=digits))
        options(scipen=100, digits=21)
    }

    if(length(x) > 1) {
        return(setNames(vapply(x, get_precision, 1, set_digits=FALSE), NULL))
    }

    x <- as.character(x)
    if(!grepl(".", x, fixed=TRUE)) return(0)

    frac <- strsplit(x, ".", fixed=TRUE)[[1]][2]
    if(is.na(frac) || nchar(frac)==0) return(0)

    digits <- strsplit(frac, "", fixed=TRUE)[[1]]

    max(which(digits != "0"))
}
