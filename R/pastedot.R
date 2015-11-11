#  paste.
#' paste with dot separator
#'
#' Calls \code{\link[base]{paste}} with \code{sep="."}.
#'
#' @param ...  Passed to paste.
#'
#' @details
#' There's not much to this function.  It just is
#'   \code{\link[base]{paste}} with \code{sep=""}, 'cause I'm lazy.
#'
#' @export
#' @return
#' A character string or vector of character strings.
#'
#' @examples
#' x <- 3
#' y <- 4
#' paste.(x, y)
#'
#' @seealso
#' \code{\link[base]{paste}},
#'   \code{\link[base]{paste0}},
#'   \code{\link{paste00}},
#'   \code{\link{paste..}},
#'   \code{\link{paste0.}},
#'   \code{\link{paste.0}}
#'
#' @keywords
#' character
paste. <- function(...) paste(..., sep=".")

#' @export
paste.. <- function(...) paste(..., sep=".", collapse=".")

#  paste00
#' paste with null or dot as separator and with collapse
#'
#' Call \code{\link[base]{paste}} with \code{sep="."} or \code{sep=""}
#'   and \code{collapse=""} or \code{collapse="."}.
#'
#' @aliases paste0. paste.0 paste..
#'
#' @param ...  Passed to paste.
#'
#' @details
#' There's not much to these functions.
#'   \code{paste00(\dots)} is like \code{paste(\dots, sep="", collapse="")}
#'   \code{paste..(\dots)} is like \code{paste(\dots, sep=".", collapse=".")}
#'   \code{paste0.(\dots)} is like \code{paste(\dots, sep="", collapse=".")}
#'   \code{paste.0(\dots)} is like \code{paste(\dots, sep=".", collapse="")}
#'
#' @export
#' @return
#' A character string or vector of character strings.
#'
#' @examples
#' x <- c(3, 4)
#' y <- c(5, 6)
#' paste00(x, y)
#' paste..(x, y)
#' paste0.(x, y)
#' paste.0(x, y)
#'
#' @seealso
#' \code{\link[base]{paste}},
#'   \code{\link[base]{paste0}},
#'   \code{\link{paste.}}
#'
#' @keywords
#' character
paste00 <- function(...) paste(..., sep="", collapse="")

#' @export
paste0. <- function(...) paste(..., sep="", collapse=".")

#' @export
paste.0 <- function(...) paste(..., sep=".", collapse="")
