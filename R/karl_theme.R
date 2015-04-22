#' Karl's ggplot2 theme
#'
#' Karl's ggplot2 theme: black border and no ticks
#'
#' @param ... Passed to \code{\link[ggplot2]{theme}}
#'
#' @return An object as returned by \code{\link[ggplot2]{theme}}
#'
#'
#' @examples
#' library(ggplot2)
#' mtcars$cyl <- factor(mtcars$cyl)
#' ggplot(mtcars, aes(y=mpg, x=disp, color=cyl)) +
#'     geom_point() + karl_theme()
#'
#' @seealso \code{\link[ggplot2]{theme}}
#'
#' @export
karl_theme <-
    function(...)
{
    ggplot2::theme(axis.ticks.length=grid::unit(0, "cm"),
                   panel.border=ggplot2::element_rect(fill=NA, color="black"),
                   ...)
}
