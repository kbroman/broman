#  get0
#'
#' get with paste
#'
#' Calls \code{\link[base]{paste}} then \code{\link[base]{get}}
#'
#' @aliases get.
#'
#' @param ... Vector of character strings.
#'
#' @details
#' There's not much to these functions.  \code{get0} uses \code{\link[base]{paste0}}
#'   and so combines its arguments with \code{sep=""}.   \code{get.} uses
#'   \code{\link[base]{paste}} with \code{sep="."}.
#'
#' @export
#'
#' @return
#' The object found.
#'
#' @author
#' Karl W Broman \email{kbroman@@biostat.wisc.edu}
#'
#' @examples
#' xy <- 3
#' x.y <- 18.3
#' get0("x", "y")
#' get.("x", "y")
#'
#' @seealso
#' \code{\link[base]{get}}, 
#'   \code{\link[base]{paste}}, 
#'   \code{\link[base]{paste0}}
#'
#' @keywords
#' data
get0 <- function(...) get(paste0(...))
get. <- function(...) get(paste(..., sep="."))
