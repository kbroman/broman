# qqline corresponding to qqplot
#  qqline2
#'
#' qqline for qqplot
#'
#' Adds a line to a quantile-quantile plot for two datasets, from \code{\link{stats}[qqplot]}.
#'   (The available \code{\link[stats]{qqline}} function works mainly for
#'   \code{\link[stats]{qqnorm}}, with one sample being theoretical quantiles.)
#'
#' @param x The first sample
#'
#' @param y The second sample.
#'
#' @param probs numeric vector of length two, representing probabilities.
#'     Corresponding quantile pairs define the line drawn.
#'
#' @param qtype the \code{type} of quantile computation used in \code{\link{quantile}}.
#'
#' @param ...  graphical parameters.
#'
#' @export
#' @importFrom graphics abline
#'
#' @return
#' Intercept and slope of the line.
#'
#' @examples
#' x <- rchisq(500, 3)
#' y <- rgamma(730, 3, 1/2)
#' qqplot(x, y)
#' qqline2(x, y)
#'
#' @seealso
#' \code{\link[stats]{qqline}}, \code{\link[stats]{qqplot}}
#'
#' @keywords
#' hplot
qqline2 <- function(x, y, probs = c(0.25, 0.75), qtype = 7, ...)
{
    stopifnot(length(probs) == 2)
    x <- quantile(x, probs, names=FALSE, type=qtype, na.rm = TRUE)
    y <- quantile(y, probs, names=FALSE, type=qtype, na.rm = TRUE)
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope*x[1L]
    abline(int, slope, ...)
    invisible(c(intercept=int, slope=slope))
}
