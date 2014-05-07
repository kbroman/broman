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
#'
#' @return
#' A character string; the input in hex.
#'
#' @author
#' Karl W Broman, \email{kbroman@@biostat.wisc.edu}
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
{
  if(length(d) > 1) {
    res <- d
    for(i in seq(along=d))
      res[i] <- convert2hex(d[i])
    return(res)
  }

  if(d != round(d) || d < 0)
    stop("d must be a non-negative integer")

  hex <- c(0:9,LETTERS[1:6])

  if(d < 16) return(hex[d+1])

  high <- floor(log(d, 16))

  res <- ""
  for(i in high:1) {
    z <- d %/% (16^i)
    res <- paste(res, hex[z + 1], sep="")
    d <- d - z*(16^i)
  }

  paste(res, hex[(d %% 16) + 1], sep="")
}

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
#'
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
{
  if(length(h) > 1) {
    res <- h
    for(i in seq(along=h))
      res[i] <- hex2dec(h[i])
    return(res)
  }

  hex <- c(0:9,LETTERS[1:6])

  if(!is.character(h)) h <- as.character(h)
  h <- toupper(h)

  hspl <- rev(unlist(strsplit(h, "")))
  hc <- match(hspl, hex)-1
  if(any(is.na(hc)))
     stop("Invalid characters (", paste(hspl[is.na(hc)], collapse=" "), ") in ", h)

  sum(hc * 16^(0:(length(hc)-1)))
}

# end of convert2hex.R
