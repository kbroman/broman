#' Value matching
#'
#' `%in%` returns logical vector indicating values that do not have a match.
#' `%win%` returns a vector of the values that have a match.
#' `%wnin%` returns a vector of the values that do not have a match.
#'
#' @md
#' @rdname notin
#'
#' @param x Vector of values to be matched.
#' @param table Vector of values to be matched against.
#'
#' @return
#' `%nin%` returns a logical vector of the same length of `x`, indicating which values are not in `table`.
#'
#' `%win%` returns a sub-vector of `x` with the values that were found in `table`.
#'
#' `%wnin%` returns a sub-vector of `x` with the values that were not found in `table`.
#'
#' @export
#' @seealso [base::match()]
#'
#' @examples
#' vals <- c("a", "xa", "b")
#' vals %nin% letters
#' vals %win% letters
#' vals %wnin% letters
'%nin%' <-  function(x, table) !(x %in% table)

#' @rdname notin
#' @export
'%win%' <- function(x, table) x[x %in% table]

#' @rdname notin
#' @export
'%wnin%' <- function(x, table) x[x %nin% table]
