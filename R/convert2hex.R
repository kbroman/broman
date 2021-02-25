#  convert2hex
#' Convert decimal to hex
#'
#' Convert a number to hexidecimal notation.
#'
#' @aliases dec2hex
#'
#' @param d A vector of integers (must be < 2^31).
#'
#' @export
#' @return
#' The input in hex, as character strings.
#'
#' @examples
#' convert2hex(333)
#' dec2hex(333)
#' dec2hex(0:30)
#'
#' @seealso [hex2dec()]
#'
#' @keywords
#' manip
convert2hex <-
    function(d)
{
    maxval <- 2^31
    if(any(!is.na(d) & d > maxval)) {
        warning("Only works for values < 2^31")
        d[!is.na(d) & d > maxval] <- NA
    }
    as.character(as.hexmode(d))
}

#' @export
dec2hex <- convert2hex


#  hex2dec
#'
#' Convert from hex to decimal
#'
#' Convert a number from hexidecimal to decimal notation.
#'
#' @param h Vector of character strings with hexadecimal representation of integers
#' (values >= 2^31 converted to missing, `NA`)
#'
#' @export
#' @return
#' The input converted from hexadecimal to decimal notation.
#'
#' @author
#' Karl W Broman, \email{broman@@wisc.edu}
#'
#' @examples
#' hex2dec("14D")
#' hex2dec(0:30)
#'
#' @seealso [dec2hex()]
#'
#' @keywords
#' manip
hex2dec <-
    function(h)
    strtoi(h, base=16)

# end of convert2hex.R
