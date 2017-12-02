#' Turn a vector into a single character string
#'
#' Turn a vector into a single character string with the items separated by commas and an "and".
#'
#' @param x A vector
#'
#' @export
#'
#' @examples
#' vec2string(letters[1:2])
#' vec2string(letters[1:4])
vec2string <-
    function(x)
{
    n <- length(x)

    if(n==0) return("")
    if(n==1) return(paste(x))
    if(n==2) return(paste(x, collapse=" and "))

    paste(paste(x[-n], collapse=", "), x[n], sep=", and ")
}
