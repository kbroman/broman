#  paste.
#' paste with dot separator
#'
#' Calls [base::paste()] with `sep="."`.
#'
#' @param ...  Passed to paste.
#'
#' @details
#' There's not much to this function.  It is just
#'   [base::paste()] with `sep="."`, because I'm lazy.
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
#' [base::paste()],
#'   [base::paste0()],
#'   [paste00()],
#'   [paste..()],
#'   [paste0.()],
#'   [paste.0()]
#'
#' @keywords
#' character
paste. <- function(...) paste(..., sep=".")

#  paste00
#' @name paste00
#' @rdname paste00
#' @aliases paste.. paste.0 paste0.
#'
#' @title paste with null or dot as separator and with collapse
#'
#' @description Call [base::paste()] with `sep="."` or `sep=""`
#'   and `collapse=""` or `collapse="."`.
#'
#' @aliases paste0. paste.0 paste..
#'
#' @param ...  Passed to paste.
#'
#' @details
#' There's not much to these functions.
#'
#' - `paste00(...)` is like `paste(..., sep="", collapse="")`
#'
#' - `paste..(...)` is like `paste(..., sep=".", collapse=".")`
#'
#' - `paste0.(...)` is like `paste(..., sep="", collapse=".")`
#'
#' - `paste.0(...)` is like `paste(..., sep=".", collapse="")`
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
#' [base::paste()],
#'   [base::paste0()],
#'   [paste.()]
#'
#' @keywords
#' character
paste00 <- function(...) paste(..., sep="", collapse="")

#' @rdname paste00
#' @export
paste.. <- function(...) paste(..., sep=".", collapse=".")

#' @rdname paste00
#' @export
paste0. <- function(...) paste(..., sep="", collapse=".")

#' @rdname paste00
#' @export
paste.0 <- function(...) paste(..., sep=".", collapse="")
