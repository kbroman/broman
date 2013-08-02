# qqline corresponding to qqplot
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
