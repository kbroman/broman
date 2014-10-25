#' Number of unique values
#'
#' Get the number of unique values in a vector
#'
#' @param vec A vector
#' @param na.rm If \code{TRUE}, remove any missing values
#'
#' @return Number of unique values.
#'
#' @details It just does \code{length(unique(vec))} or, if
#' \code{na.rm=TRUE} (the default)
#' \code{length(unique(vec[!is.na(vec)]))}
#'
#' @export
#' @keywords utilities
#' @examples
#' x <- c(1, 2, 1, 3, 1, 1, 2, 2, 3, NA, NA, 1)
#' lenuniq(x)
#' lenuniq(x, na.rm=FALSE)

lenuniq <-
    function(vec, na.rm=TRUE)
{
    if(na.rm && !is.null(vec)) vec <- vec[!is.na(vec)]
    length(unique(vec))
}
