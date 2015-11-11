#  myround
#'
#' Round a number, preserving extra 0's
#'
#' Round a number, preserving extra 0's.
#'
#' @param x Number to round.
#'
#' @param digits Number of digits past the decimal point to keep.
#'
#' @details
#' Uses \code{\link[base]{sprintf}} to round a number, keeping extra 0's.
#'
#' @export
#' @return
#' A vector of character strings.
#'
#' @examples
#' myround(51.01, 3)
#' myround(0.199, 2)
#'
#' @seealso
#' \code{\link[base]{round}}, \code{\link[base]{sprintf}}
#'
#' @keywords
#' utilities
myround <-
    function(x, digits=1)
{
    if(digits < 1)
        stop("This is intended for the case digits >= 1.")

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)

    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero

    tmp
}
