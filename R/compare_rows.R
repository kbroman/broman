# compare_rows
#' Compare rows in a matrix
#'
#' For all pairs of rows in a matrix, calculate the proportion of mismatches or the RMS difference.
#'
#' @param mat Numeric matrix. Should be integers in the case \code{method="prop_mismatches"}.
#' @param method Indicates whether to use proportion mismatches or the
#' RMS difference. Missing values are omitted.
#'
#' @export
#' @return A square matrix of dimension \code{nrow(mat)} with
#' \code{NA}s on the diagonal and the calculated statistic in the
#' body.
#'
#' @examples
#' n <- 10
#' p <- 200
#' x <- matrix(sample(1:4, n*p, replace=TRUE), ncol=p)
#' d <- compare_rows(x)

compare_rows <-
    function(mat, method=c("prop_mismatches", "rms_difference"))
{

    method <- match.arg(method)

    if(!is.matrix(mat))
        stop("mat should be a matrix")

    n <- nrow(mat)
    p <- ncol(mat)

    if(method=="prop_mismatches") {
        z <- .C("R_compare_rows_mismatch",
                as.integer(mat),
                as.integer(n),
                as.integer(p),
                d=as.double(rep(0, n*n)),
                NAOK=TRUE,
                PACKAGE="broman")
    } else {
        z <- .C("R_compare_rows_rmsd",
                as.double(mat),
                as.integer(n),
                as.integer(p),
                d=as.double(rep(0, n*n)),
                NAOK=TRUE,
                PACKAGE="broman")
    }

    d <- matrix(z$d, nrow=n, ncol=n)
    diag(d) <- NA
    dimnames(d) <- list(rownames(mat), rownames(mat))

    d
}
