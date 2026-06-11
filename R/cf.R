#  cf
#' Compare objects, including missing data pattern
#'
#' Check whether two objects are the same, including their patterns of `NA`s.
#'
#' @aliases cf.default cf.list
#'
#' @param a Some object.
#'
#' @param b Another object
#'
#' @param tol Tolerance value. If NULL, we use the comparison `a==b`; if not NULL we use `max(a-b)<tol`.
#'
#' @details
#' It's not very complicated: `((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b))`
#'
#' @export
#' @return
#' Boolean object with `TRUE` indicating an element is the same.
#'
#' @examples
#' x <- c(5, 8, 9, NA, 3, NA)
#' y <- c(5, 2, 9, 4, NA, NA)
#' cf(x,y)
#'
#' x <- matrix(rnorm(1000), ncol=20)
#' x[sample(seq(along=x), 100)] <- NA
#' all(cf(x,x))
#' dim(cf(x,x))
#'
#' y <- x
#' y[4,8] <- NA
#' sum(!cf(x,y))
#' y[6,2] <- 18
#' sum(!cf(x,y))
#' y[6,5] <- 32
#' sum(!cf(x,y))
#'
#' x <- as.data.frame(x)
#' y <- as.data.frame(y)
#' sum(!cf(x,y))
#'
#' x <- as.list(x)
#' y <- as.list(y)
#' sapply(cf(x,y), function(a) sum(!a))
#'
#' @keywords
#' data
cf <- function(a, b, tol=NULL) UseMethod("cf")

#' @export
cf.default <-
    function(a, b, tol=NULL)
{
    if(is.null(tol)) return( ((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)) )

    ((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < tol) )
}

#' @export
cf.list <-
    function(a,b, tol=NULL)
{
    for(i in seq(along=a)) a[[i]] <- cf(a[[i]], b[[i]], tol)
    a
}
