#' Align two vectors
#'
#' Align two vectors using their names attributes, either expanding with NAs or reducing to the common values.
#'
#' @md
#'
#' @param x A vector
#' @param y Another vector
#' @param expand If TRUE, expand each to the same length using NAs. If FALSE, remove elements not in common.
#'
#' @return A list with two components, `x` and `y`
#'
#' @importFrom stats setNames
#' @export
align_vectors <-
    function(x, y, expand=TRUE)
{
    nx <- names(x)
    ny <- names(y)
    if(is.null(nx) || is.null(ny))
        stop("Need names attributes for both x and y")

    if(length(unique(nx)) != length(nx))
        stop("names(x) not all distinct")
    if(length(unique(ny)) != length(ny))
        stop("names(y) not all distinct")

    if(expand) {
        nfull <- unique(c(nx, ny))
        if(any(!(nfull %in% nx)))
            x <- c(x, setNames(rep(NA, length(nfull)-length(nx)),
                               nfull[!(nfull %in% nx)]))
        if(any(!(nfull %in% ny)))
            y <- c(y, setNames(rep(NA, length(nfull)-length(ny)),
                               nfull[!(nfull %in% ny)]))
    }

    nx <- names(x)
    ny <- names(y)
    common <- nx[nx %in% ny]
    list(x=x[common], y=y[common])
}
