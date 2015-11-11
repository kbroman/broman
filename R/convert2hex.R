#  convert2hex
#' Convert decimal to hex
#'
#' Convert a number to hexidecimal notation.
#'
#' @aliases dec2hex
#'
#' @param d A number.
#'
#' @details
#' Nothing important to say here.
#'
#' @export
#' @return
#' A character string; the input in hex.
#'
#' @examples
#' convert2hex(333)
#' dec2hex(333)
#' dec2hex(333) == "14D"
#' dec2hex(0:30)
#'
#' @seealso
#' \code{\link{hex2dec}}
#'
#' @keywords
#' manip
convert2hex <-
    function(d)
    as.character(as.hexmode(d))

#' @export
dec2hex <- convert2hex


#  hex2dec
#'
#' Convert from hex to decimal
#'
#' Convert a number from hexidecimal to decimal notation.
#'
#' @param h #' Character string with hexadecimal representation of a number
#'
#' @details
#' Nothing important to say here.
#'
#' @export
#' @return
#' The input converted from hexadecimal to decimal notation.
#'
#' @author
#' Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' hex2dec("14D")
#' hex2dec("14D") == 333
#' hex2dec(0:30)
#'
#' @seealso
#' \code{\link{dec2hex}}
#'
#' @keywords
#' manip
hex2dec <-
    function(h)
    strtoi(h, base=16)

# end of convert2hex.R
