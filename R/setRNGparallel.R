#' Set up random number generation for parallel calculations
#'
#' Set random number generation to L'Ecuyer-CMRG, for use in parallel calculations.
#'
#' @details I can never remember the command \code{RNGkind("L'Ecuyer-CMRG")}; this is a shortcut.
#' \code{unsetRNG4parallel} sets the random number generator back to the default type.
#'
#' @export
#' @keywords utilities
#'
#' @examples
#' RNGkind()
#' setRNGparallel()
#' RNGkind()
#' unsetRNGparallel()
#' RNGkind()
setRNGparallel <-
    function()
{
    RNGkind("L'Ecuyer-CMRG")
}


#' @rdname setRNGparallel
#' @export
unsetRNGparallel <-
    function()
{
    RNGkind("Mersenne-Twister")
}
