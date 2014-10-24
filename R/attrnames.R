#' Get names of attributes
#'
#' Get the names of the attributes of an object
#'
#' @param object Any object
#'
#' @return Vector of character strings with the names of the attributes.
#'
#' @details It just does \code{names(attributes(object))}.
#'
#' @export
#' @keywords utilities
#' @examples
#' x <- matrix(1:100, ncol=5)
#' colnames(x) <- LETTERS[1:5]
#' attrnames(x)

attrnames <-
    function(object) names(attributes(object))
