## kbdate
## Karl Broman

#' My only little date facility
#'
#' Sys.Date as a string, in a few different formats
#'
#' @param format The format for the output
#'
#' @param date The date/time to convert
#'
#' @export
#' @return A character string representation of the date/time
#'
#' @examples
#' kbdate()
#' kbdate("standard")
#'
#' @seealso \code{\link[base]{Sys.time}}, \code{\link[base]{date}}
#'
#' @keywords utilities
kbdate <-
    function(format=c("dateonly", "standard"), date=Sys.time())
{
    format <- match.arg(format)
    base::format(date,
                 switch(format,
                        dateonly="%Y-%m-%d",
                        standard="%a %b %d %X %Y"))
}
