# add_commas
#' Add commas to a large number
#'
#' Convert a number to a string, with commas every 3rd digit
#'
#' @param numbers Vector of non-negative numbers (will be rounded to integers)
#'
#' @export
#' @return Character string with numbers written like \code{"7,547,085"}.
#'
#' @examples
#' add_commas(c(231, 91310, 2123, 9911001020, 999723285))
add_commas <-
    function(numbers)
{
    format(numbers, big.mark=",", scientific=FALSE, trim=TRUE)
}
