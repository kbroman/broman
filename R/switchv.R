# switchv
#' Vectorized version of switch
#'
#' Vectorized version of \code{\link[base]{switch}}: just loops over
#' input and calls \code{\link[base]{switch}}.
#'
#' @param EXPR An expression evaluating to a vector of numbers of strings
#' @param ... List of alternatives
#'
#' @return Vector of returned values.
#'
#' @examples
#' switchv(c("horse", "fish", "cat", "bug"),
#'         horse="fast",
#'         cat="cute",
#'         "what?")
#'
#' @export
switchv <- function(EXPR, ...) {
    result <- EXPR

    for(i in seq(along=result))
        result[i] <- switch(EXPR[i], ...)

    result
}
