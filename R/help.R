#  h
#'
#' View html version of help file
#'
#' View the html version of a help file while running R via ESS within emacs.
#'
#' @param ... Help topics.
#'
#' @details
#' This just calls the function [utils::help()] using the
#'   argument `htmlhelp=TRUE`.
#'
#' @export
#' @return
#' No return value.
#'
#' @examples
#' h(read.cross)
#'
#' @seealso [utils::help()], [utils::help.start()]
#'
#' @keywords
#' documentation
h <-
    function(...)
{
    utils::help(..., help_type="html")
}
