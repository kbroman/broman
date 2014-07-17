add_border <-
function(col="black", ...)
{
    u <- par("usr")
    rect(u[1], u[3], u[2], u[4], col=col, ...)
    invisible(return(u))
}
