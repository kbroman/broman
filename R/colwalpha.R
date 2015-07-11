## colwalpha
##
#' Convert a color to use alpha transparency
#'
#' Convert a color to RGB and then to RGB with alpha transparency
#'
#' @param color A character string for a color
#' @param alpha Traparency value (between 0 and 1)
#'
#' @return A character string representing a color
#'
#' @importFrom grDevices rgb col2rgb
#' @export
#' @keywords color
#' @examples
#' colwalpha(c("blue", "red"), 0.5)
colwalpha <-
    function(color, alpha=1)
{
    stopifnot(alpha >=0, alpha <= 1)
    alpha <- alpha * 255

    rgbval <- col2rgb(color)
    rgb(rgbval[1,], rgbval[2,], rgbval[3,], alpha=alpha, maxColorValue=255)
}
