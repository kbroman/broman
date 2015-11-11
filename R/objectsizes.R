#  objectsizes
#'
#' Calculate sizes of all objects in workspace
#'
#' Calculate the sizes of all of the objects in one's workspace.
#'
#' @param obj Vector of object names.  If missing, we pull out all object
#'   names.
#'
#' @param sortbysize If TRUE, sort the objects from smallest to largest.
#'
#' @details
#' Calls \code{\link[utils]{object.size}} repeated to get the size of a
#'   list of objects.
#'
#' @export
#' @return
#' A data frame with the only column being the size of each object in
#'   megabytes (Mb).  The row names are the names of the objects.
#'
#' @examples
#' print(output <- objectsizes())
#' \dontrun{sum(output)}
#'
#' @seealso
#' \code{\link[utils]{object.size}},
#'   \code{\link[base]{objects}}
#'
#' @keywords
#' utilities
objectsizes <-
    function(obj, sortbysize=TRUE)
{
    if(missing(obj)) obj <- objects(pos=1)
    result <- data.frame(Mb=rep(0, length(obj)))
    rownames(result) <- obj
    for(i in seq(along=obj))
        result[i,1] <- utils::object.size(get(obj[i], pos=1))/1024^2
    if(sortbysize) result <- result[order(result[,1], decreasing=FALSE),,drop=FALSE]
    result
}
