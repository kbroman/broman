# add_commas
#' Add commas to a large number
#'
#' Convert a number to a string, with commas every 3rd digit
#'
#' @param numbers Vector of non-negative numbers (will be rounded to integers)
#'
#' @export
#'
#' @return Character string with numbers written like \code{"7,547,085"}.
#'
#' @examples
#' add_commas(c(231.3, 91310.2, 2123.8, 9911001020, 999723285))
add_commas <-
    function(numbers)
{
    str <- as.character(round(as.numeric(numbers)))
    spl <- strsplit(str, "")
    spl <- lapply(spl, rev)
    spl <- lapply(spl, function(a) {
        b <- NULL
        for(i in seq(1, length(a), by=3)) {
            if(!is.null(b)) b <- c(b, ",")
            b <- c(b, a[i:min(c(i+2, length(a)))])
        }
        paste(rev(b), collapse="") })

    unlist(spl)
}
