#' Spell out an integer
#'
#' Spell out an integer as a word, for use in reports/papers.
#'
#' @param number A number that is to be spelled out (can be a vector).
#' @param capitalize If TRUE, capitalize the first letter.
#' @param max_value Maximum value to use (generally 9); if larger than this, use numerals.
#'
#' @return Character string (or vector of character strings) with numbers spelled out, or as numerals if large.
#'
#' @examples
#' spell_out(9)
#' spell_out(9, cap=TRUE)
#' spell_out(9, max_value=5)

#' @export
spell_out <-
    function(number, capitalize=FALSE, max_value=9)
{
    if(length(number) > 1) {
        return( sapply(number, spell_out, capitalize=capitalize, max_value=max_value) )
    }

    if(abs(number - round(number)) > 1e-6) {
        warning("Number ", number, " has been rounded to the nearest integer")
    }
    number <- round(number)

    if(number > max_value || number > 20 || number < 0) {
        if(max_value > 20) warning("Ignoring max_value if >20")
        return(as.character(number))
    }

    if(capitalize) {
        if(number==0) return("Zero")
        return(broman::Numbers[number])
    }

    if(number==0) return("zero")
    broman::numbers[number]
}
