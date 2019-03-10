#  triplot
#'
#' Plot Holmans triangle
#'
#' Plot Holmans triangle (an equilateral triangle used to depict
#'   trinomial distributions).
#'
#' @param labels Labels for the three corners (lower-right, top, lower-left).
#'
#' @param col Color of edges of triangle
#'
#' @param lwd Line width for edges of triangle
#'
#' @param bgcolor Background color for triangle
#'
#' @param ... Passed to [graphics::plot()].
#'
#' @details
#' Plot of an equilateral triangle, in order to depict trinomial
#'   distributions.  A trinomial distribution (that is, a trio of
#'   non-negative numbers that add to 1) is equated to a point in the
#'   triangle through the distances to the three sides.  This makes use of
#'   the fact that for any point in an equilateral triangle, the sum of the
#'   distances to the three sides is constant.
#'   The `triplot` function creates an empty triangle for use with the
#'   related functions [tripoints()], [trilines()],
#'   [triarrow()].
#'
#' @importFrom graphics plot par text polygon points lines arrows
#' @export
#' @return
#' The (x,y) coordinates of the points plotted, if any.
#'
#' @examples
#' triplot()
#' x <- cbind(c(0.9, 0.05, 0.05), c(0.8, 0.1, 0.1), c(0.1, 0.9, 0), c(0, 0.9, 0.1))
#' tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)
#' trilines(x, lwd=2, col="orange")
#' y <- cbind(c(0.05, 0.05, 0.9), c(0.25, 0.25, 0.5))
#' triarrow(y, col="blue", lwd=2, len=0.1)
#'
#' @seealso
#' [tripoints()], [trilines()],
#'   [triarrow()], [tritext()]
#'
#' @keywords
#' hplot
triplot <-
    function(labels=c("(1,0,0)", "(0,1,0)", "(0,0,1)"),
             col="black", lwd=2, bgcolor="gray90", ...)
{
    m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

    pts <- m %*% diag(rep(1,3))
    lim <- apply(pts,1,range)
    rlim <- apply(lim,2,diff)
    lim[1,] <- lim[1,] - rlim*0.15
    lim[2,] <- lim[2,] + rlim*0.15

    plot(0, 0, type="n", xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=lim[,1], ylim=lim[,2], xaxs="i", yaxs="i", ...)

    pin <- par("pin")
    if(pin[2] > pin[1])
        pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
    else
        pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2

    for(i in 1:2) pts[i,] <- pts[i,] - mean(range(pts[i,])) + mean(range(lim[,i]))

    ya <- c(0,rlim[2]*0.06,0)
    xa <- c(rlim[1],0,-rlim[1])*0.06
    for(i in 1:3)
        text(pts[1,i]+xa[i], pts[2,i]+ya[i], labels[i])

    polygon(c(pts[1,], pts[1,1]), c(pts[2,], pts[2,1]),
            border=col, col=bgcolor, lwd=lwd)

    invisible(pts)
}


#  tripoints
#'
#' Plot points within a Holmans triangle
#'
#' Plot points within a Holmans triangle (an equilateral triangle used to depict
#'   trinomial distributions).
#'
#' @param x A matrix with three rows, each column being a trinomial distribution.
#'
#' @param ... Passed to [graphics::points()].
#'
#' @details
#' Plot of an equilateral triangle, in order to depict trinomial
#'   distributions.  A trinomial distribution (that is, a trio of
#'   non-negative numbers that add to 1) is equated to a point in the
#'   triangle through the distances to the three sides.  This makes use of
#'   the fact that for any point in an equilateral triangle, the sum of the
#'   distances to the three sides is constant.
#'   First use [triplot()] to first plot the equilateral triangle.
#'
#' @export
#' @return
#' The (x,y) coordinates of the points plotted.
#'
#' @examples
#' triplot()
#' x <- cbind(c(0.9, 0.05, 0.05), c(0.8, 0.1, 0.1), c(0.1, 0.9, 0), c(0, 0.9, 0.1))
#' tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)
#' trilines(x, lwd=2, col="orange")
#' y <- cbind(c(0.05, 0.05, 0.9), c(0.25, 0.25, 0.5))
#' triarrow(y, col="blue", lwd=2, len=0.1)
#'
#' @seealso
#' [triplot()], [trilines()],
#'   [triarrow()], [tritext()]
#'
#' @keywords
#' hplot
tripoints <-
    function(x, ...)
{
    m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

    pts <- m %*% diag(rep(1,3))
    lim <- apply(pts,1,range)
    rlim <- apply(lim,2,diff)
    lim[1,] <- lim[1,] - rlim*0.12
    lim[2,] <- lim[2,] + rlim*0.12

    x <- as.matrix(x)
    if(is.matrix(x) && nrow(x) != 3) x <- t(x)
    if(any(abs(colSums(x) - 1) > 1e-6)) {
        x <- x / colSums(x)
        warning("Some columns do not sum to 1; rescaling.")
    }

    x <- t(m %*% x)
    pin <- par("pin")
    if(pin[2] > pin[1]) {
        pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
        x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
    }
    else {
        x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
        pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
    }
    for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

    points(x, ...)
    invisible(x)
}

#  trilines
#'
#' Plot lines within a Holmans triangle
#'
#' Plot lines within a Holmans triangle (an equilateral triangle used to depict
#'   trinomial distributions).
#'
#' @param x A matrix with three rows, each column being a trinomial
#'   distribution.  Lines between these points are plotted.
#'
#' @param ... Passed to [graphics::lines()].
#'
#' @details
#' Plot of an equilateral triangle, in order to depict trinomial
#'   distributions.  A trinomial distribution (that is, a trio of
#'   non-negative numbers that add to 1) is equated to a point in the
#'   triangle through the distances to the three sides.  This makes use of
#'   the fact that for any point in an equilateral triangle, the sum of the
#'   distances to the three sides is constant.
#'   First use [triplot()] to first plot the equilateral triangle.
#'
#' @export
#' @return
#' The (x,y) coordinates of the endpoints of the lines plotted.
#'
#' @examples
#' triplot()
#' x <- cbind(c(0.9, 0.05, 0.05), c(0.8, 0.1, 0.1), c(0.1, 0.9, 0), c(0, 0.9, 0.1))
#' tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)
#' trilines(x, lwd=2, col="orange")
#' y <- cbind(c(0.05, 0.05, 0.9), c(0.25, 0.25, 0.5))
#' triarrow(y, col="blue", lwd=2, len=0.1)
#'
#' @seealso
#' [triplot()], [tripoints()],
#'   [triarrow()], [tritext()]
#'
#' @keywords
#' hplot
trilines <-
    function(x, ...)
{
    m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

    pts <- m %*% diag(rep(1,3))
    lim <- apply(pts,1,range)
    rlim <- apply(lim,2,diff)
    lim[1,] <- lim[1,] - rlim*0.12
    lim[2,] <- lim[2,] + rlim*0.12

    x <- as.matrix(x)
    if(is.matrix(x) && nrow(x) != 3) x <- t(x)
    if(any(abs(colSums(x) - 1) > 1e-6)) {
        x <- x / colSums(x)
        warning("Some columns do not sum to 1; rescaling.")
    }

    x <- t(m %*% x)
    pin <- par("pin")
    if(pin[2] > pin[1]) {
        pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
        x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
    }
    else {
        x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
        pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
    }
    for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

    lines(x, ...)
    invisible(x)
}

#  triarrow
#'
#' Plot an arrow within a Holmans triangle
#'
#' Plot an arrow within a Holmans triangle (an equilateral triangle used to depict
#'   trinomial distributions).
#'
#' @param x A matrix with three rows and two columns, each column being a trinomial
#'   distribution.  An arrow between the two points is plotted.
#'
#' @param ... Passed to [graphics::arrows()].
#'
#' @details
#' Plot of an equilateral triangle, in order to depict trinomial
#'   distributions.  A trinomial distribution (that is, a trio of
#'   non-negative numbers that add to 1) is equated to a point in the
#'   triangle through the distances to the three sides.  This makes use of
#'   the fact that for any point in an equilateral triangle, the sum of the
#'   distances to the three sides is constant.
#'   First use [triplot()] to first plot the equilateral triangle.
#'
#' @export
#' @return
#' The (x,y) coordinates of the endpoints of the arrows plotted.
#'
#' @examples
#' triplot()
#' x <- cbind(c(0.9, 0.05, 0.05), c(0.8, 0.1, 0.1), c(0.1, 0.9, 0), c(0, 0.9, 0.1))
#' tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)
#' trilines(x, lwd=2, col="orange")
#' y <- cbind(c(0.05, 0.05, 0.9), c(0.25, 0.25, 0.5))
#' triarrow(y, col="blue", lwd=2, len=0.1)
#'
#' @seealso
#' [triplot()], [tripoints()],
#'   [trilines()], [tritext()]
#'
#' @keywords
#' hplot
triarrow <-
    function(x, ...)
{
    if(nrow(x) == 2 && ncol(x) == 3)
        x <- t(x)
    else if(!(nrow(x)==3 && ncol(x)==2))
        stop("x must be a 2x3 or 3x2 matrix")

    m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

    pts <- m %*% diag(rep(1,3))
    lim <- apply(pts,1,range)
    rlim <- apply(lim,2,diff)
    lim[1,] <- lim[1,] - rlim*0.12
    lim[2,] <- lim[2,] + rlim*0.12

    x <- as.matrix(x)
    if(any(abs(colSums(x) - 1) > 1e-6)) {
        x <- x / colSums(x)
        warning("Some columns do not sum to 1; rescaling.")
    }

    x <- t(m %*% x)
    pin <- par("pin")
    if(pin[2] > pin[1]) {
        pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
        x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
    }
    else {
        x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
        pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
    }
    for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

    arrows(x[1,1], x[1,2], x[2,1], x[2,2], ...)
    invisible(x)
}


#  tritext
#'
#' Plot text within a Holmans triangle
#'
#' Plot text within a Holmans triangle (an equilateral triangle used to depict
#'   trinomial distributions).
#'
#' @param x A matrix with three rows, each column being a trinomial distribution.
#'
#' @param labels A vector of character strings, with length equal to the number of columns of `x`.
#'
#' @param ... Passed to [graphics::text()].
#'
#' @details
#' Plot of an equilateral triangle, in order to depict trinomial
#' distributions.  A trinomial distribution (that is, a trio of
#' non-negative numbers that add to 1) is equated to a point in the
#' triangle through the distances to the three sides.  This makes use of
#' the fact that for any point in an equilateral triangle, the sum of the
#' distances to the three sides is constant.
#' First use [triplot()] to first plot the equilateral triangle.
#'
#' @export
#' @return
#' Text is plotted at the (x,y) coordinates of the points.
#'
#' @examples
#' triplot()
#' x <- cbind(c(0.25, 0.5, 0.25), c(1/3, 1/3, 1/3))
#' tripoints(x, lwd=2, pch=21, bg="lightblue")
#' xp <- x + c(0.02, 0, -0.02)
#' tritext(xp, c("(1/4,1/2,1/4)", "(1/3,1/3,1/3)"), adj=c(0, 0.5))
#'
#' @seealso
#' [triplot()], [trilines()],
#'   [triarrow()], [tripoints()]
#'
#' @keywords
#' hplot
tritext <-
    function(x, labels, ...)
{

    m <- rbind(c(2/sqrt(3), 1/sqrt(3), 0), c(0,1,0))

    pts <- m %*% diag(rep(1,3))
    lim <- apply(pts,1,range)
    rlim <- apply(lim,2,diff)
    lim[1,] <- lim[1,] - rlim*0.12
    lim[2,] <- lim[2,] + rlim*0.12

    x <- as.matrix(x)
    if(is.matrix(x) && nrow(x) != 3) x <- t(x)
    stopifnot(ncol(x) == length(labels))
    if(any(abs(colSums(x) - 1) > 1e-6)) {
        x <- x / colSums(x)
        warning("Some columns do not sum to 1; rescaling.")
    }

    x <- t(m %*% x)
    pin <- par("pin")
    if(pin[2] > pin[1]) {
        pts[2,] <- pts[2,] / pin[2] * pin[1]*sqrt(3)/2
        x[,2] <- x[,2] / pin[2] * pin[1]*sqrt(3)/2
    }
    else {
        x[,1] <- x[,1] / pin[1] * pin[2]/sqrt(3)*2
        pts[1,] <- pts[1,] / pin[1] * pin[2]/sqrt(3)*2
    }
    for(i in 1:2) x[,i] <- x[,i] - mean(range(pts[i,])) + mean(range(lim[,i]))

    text(x, labels, ...)
    invisible(x)
}
