#' Excel-style figure displaying contents of a matrix
#'
#' Turn a matrix of data into an SVG of how it might look in Excel
#'
#' @param mat A matrix
#' @param file Optional file name (must have extension .svg, .png, .jpg, or .pdf)
#' @param cellwidth Width of each cell, in pixels
#' @param cellheight Height of each cell, in pixels
#' @param textsize Size for text (if \code{file} is provided or \code{direct2svg=TRUE})
#' @param fig_width Width of figure, in pixels (if missing, taken from \code{cellwidth}); ignored when \code{direct2svg=FALSE}
#' @param fig_height Height of figure, in pixels (if missing, taken from \code{cellheight}); ignored when \code{direct2svg=FALSE}
#' @param border Color of border of cells for the body of the matrix
#' @param headcol Background color of cells on the top and left border
#' @param headborder Color of border of cells on the top and left border
#' @param headtextcol Color of text in cells on the top and left border
#' @param textcol Color of text in in cells in body of the matrix
#' @param row_names If TRUE, and row names are present, include them as a first column
#' @param col_names If TRUE, and column names are present, include them as a first row
#' @param hilitcells Character vector of cells to highlight, like \code{"A1"} or \code{"D4"}
#' @param hilitcolor Color to highlight cells, a vector of length 1 or the same length as \code{hilitcells}
#' @param lwd Line width for rectangles
#' @param direct2svg If TRUE, rather than R graphics, just print an SVG directly with \code{\link[base]{cat}}.
#'
#' @export
#' @importFrom grDevices svg png jpeg pdf
#' @importFrom graphics plot rect text par
#' @keywords hplot
#'
#' @examples
#' df <- data.frame(id=    c(101,  102,  103),
#'                  sex=   c("M",  "F",  "M"),
#'                  weight=c(22.3, 15.8, 19.7),
#'                  stringsAsFactors=FALSE)
#' excel_fig(df, col_names=TRUE)
excel_fig <-
    function(mat, file, cellwidth=80, cellheight=26, textsize=16,
             fig_width, fig_height,
             border="#CECECE", headcol="#E9E9E9", headborder="#969696",
             headtextcol="#626262", textcol="black",
             row_names=FALSE, col_names=TRUE,
             hilitcells, hilitcolor="#F0DCDB", lwd=1,
             direct2svg=FALSE)

{

    if(row_names && !is.null(rownames(mat)))
        mat <- cbind("row"=row_names, mat, stringsAsFactors=FALSE)
    if(col_names && !is.null(colnames(mat)))
        mat <- rbind(colnames(mat), mat)

    n_row <- nrow(mat)
    n_col <- ncol(mat)
    if(length(cellheight)==1)
        cellheight <- rep(cellheight, n_row + 1)
    if(length(cellheight) != n_row+1)
        stop("cellheight should have length 1 or ", n_row+1)
    if(length(cellwidth)==1)
        cellwidth <- rep(cellwidth, n_col + 1)
    if(length(cellwidth) != n_col+1)
        stop("cellwidth should have length 1 or ", n_col+1)

    height <- sum(cellheight)
    width <- sum(cellwidth)
    if(missing(fig_height)) fig_height <- height
    if(missing(fig_width)) fig_width <- width
    celly <- cumsum(c(0,cellheight))
    cellx <- cumsum(c(0,cellwidth))

    if(!missing(file) && !is.null(file)) {
        if(grepl("\\.svg$", file))
            svg(file, width=width/72, height=height/72, pointsize=textsize)
        else if(grepl("\\.png$", file))
            png(file, width=width, height=height, pointsize=textsize)
        else if(grepl("\\.pdf$", file))
            pdf(file, width=width/72, height=height/72, pointsize=textsize)
        else if(grepl("\\.jpg$", file))
            jpeg(file, width=width, height=height, pointsize=textsize)
        else
            stop("file must have extension .svg, .png, .jpg, or .pdf")
    } else { # if missing, make it NULL
        file <- NULL
    }


    # matrix containing color of cells
    colormat <- matrix("white", nrow=nrow(mat), ncol=ncol(mat))

    if(!missing(hilitcells) && !is.null(hilitcells)) {

        # make sure hilitcolor is a vector of same length as hilitcells
        if(length(hilitcolor) == 1)
            hilitcolor <- rep(hilitcolor, length(hilitcells))
        else if(length(hilitcolor) != length(hilitcells))
            stop("length(hilitcolor) (", length(hilitcolor),
                 ") != length(hilitcells) (", length(hilitcells), ")")

        hilitcells <- toupper(hilitcells) # to upper-case
        hilitcells <- strsplit(hilitcells, "") # split into characters

        # check that cells are valid ("A4" "D05", etc)
        sapply(hilitcells, function(a) {
               let <- which(a %in% LETTERS)
               num <- which(!(a %in% LETTERS))
               if(length(let) == 0 || length(num) == 0 ||
                  any(sapply(let, function(a,b) any(a > b), num)))
                   stop("Invalid highlighted cell: ", paste(a, collapse=""))
           })


        # pull out letters and convert to column number
        col <- vapply(hilitcells, function(a) {
            b <- a[a %in% LETTERS] # pull out the letters
            m <- match(b, LETTERS) # convert to digits
            if(length(b) == 1) return(m) # if 1 letter, just use corresponding number
            sum((26^((length(b)-1):0))*m) # otherwise, treat like base 26, offset by 26
        }, 1)

        # pull out the digits and convert back to row number
        row <- vapply(hilitcells, function(a) as.numeric(paste(a[!(a %in% LETTERS)], collapse="")), 1)

        # make a matrix
        hilitcells <- cbind(row, col)

        # change color of highlighted cells
        for(i in 1:nrow(hilitcells))
            colormat[hilitcells[i,1], hilitcells[i,2]] <- hilitcolor[i]
    }


    if(direct2svg) {
        return( excel_fig_direct(mat, file,
                                 cellx, celly, cellwidth, cellheight, width, height,
                                 fig_width, fig_height,
                                 textsize, border, headcol, headborder,
                                 headtextcol, textcol,
                                 colormat, lwd) )
    }


    # plot region
    par(mar=rep(0.1, 4))
    plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=c(0, width), ylim=c(height, 0), xaxs="i", yaxs="i")



    for(i in n_row:0) {
        for(j in n_col:0) {
            # rectangles
            rect(cellx[j+1], celly[i+1], cellx[j+2], celly[i+2],
                 border=ifelse(i==0 || j==0, headborder, border),
                 col=ifelse(i==0 || j==0, headcol, colormat[i,j]),
                 lwd=lwd)

            # text
            if(i==0 && j>0)
                text(mean(cellx[j+1:2]), mean(celly[1:2]), LETTERS[j], col=headtextcol, font=2)
            if(i>0 && j==0)
                text(mean(cellx[1:2]), mean(celly[i+1:2]), i, col=headtextcol, font=2)
            if(i>0 && j>0)
                text(mean(cellx[j+1:2]), mean(celly[i+1:2]), mat[i,j], col=textcol)
        }
    }

    if(!missing(file) && !is.null(file))
        grDevices::dev.off()
}

# a grid version, for internal use
excel_fig_direct <-
    function(mat, file, cellx, celly, cellwidth, cellheight, width, height,
             fig_width, fig_height,
             textsize, border, headcol, headborder, headtextcol, textcol,
             colormat, lwd)
{
    # initiate the svg, padding things a bit
    svg_start(width+lwd*2, height+lwd*2, fig_width+lwd*2, fig_height+lwd*2, file)

    # shift a bit, due to padding
    cellx <- cellx+lwd
    celly <- celly+lwd

    n_row <- nrow(mat)
    n_col <- ncol(mat)
    for(i in n_row:0) {
        for(j in n_col:0) {
            # rectangles
            svg_rect(cellx[j+1], celly[i+1], cellwidth[j+1], cellheight[i+1],
                     border=ifelse(i==0 || j==0, headborder, border),
                     col=ifelse(i==0 || j==0, headcol, colormat[i,j]),
                     lwd=lwd, file=file)

            # text
            if(i==0 && j>0)
                svg_text(mean(cellx[j+1:2]), mean(celly[1:2]), LETTERS[j],
                         col=headtextcol, textsize=textsize, file=file)
            if(i>0 && j==0)
                svg_text(mean(cellx[1:2]), mean(celly[i+1:2]), i,
                         col=headtextcol, textsize=textsize, file=file)
            if(i>0 && j>0)
                svg_text(mean(cellx[j+1:2]), mean(celly[i+1:2]), mat[i,j],
                         col=headtextcol, textsize=textsize, file=file)
        }
    }

    # close off the svg
    svg_end(file)
}

# use cat() to file if given otherwise plain
svg_cat <-
    function(..., file=NULL, append=TRUE)
{
    if(missing(file) || is.null(file))
        cat(..., sep="")
    else
        cat(..., file=file, append=append, sep="")
}

# print the start of an SVG
svg_start <-
    function(width, height, fig_width, fig_height, file=NULL)
{
    svg_cat('<?xml version="1.0" encoding="UTF-8"?>\n', file=file, append=FALSE)
    svg_cat('<svg ',
            'width="', fig_width, 'px" ',
            'height="', fig_height, 'px" ',
            'viewBox="0 0 ', width, ' ', height, '" ',
            'preserveAspectRatio="xMinYmin meet" ',
            'xmlns="http://www.w3.org/2000/svg" ',
            'xmlns:xlink="http://www.w3.org/1999/xlink" ',
            'version="1.1">\n',
            file=file, append=FALSE)
}

# print the end of an SVG
svg_end <-
    function(file=NULL)
{
    svg_cat('</svg>\n', file=file, append=TRUE)
}

# add a rectangle to an SVG
svg_rect <-
    function(x, y, width, height, col="white", border="black", lwd=1,
             file=NULL)
{
    svg_cat('    <rect ',
            'x="', x, '" ',
            'y="', y, '" ',
            'width="', width, '" ',
            'height="', height, '" ',
            'fill="', col, '" ',
            'stroke="', border, '" ',
            'stroke-width="', lwd, '" ',
            '/>\n', file=file, append=TRUE)
}

# print text
svg_text <-
    function(x, y, text, col="white", textsize=14, file=NULL)
{
    svg_cat('    <text ',
            'x="', x, '" ',
            'y="', y, '" ',
            'text-anchor="middle" ',
            'dominant-baseline="middle" ',
            'font-family="sans-serif" ',
            'fill="', col, '" ',
            'font-size="', textsize, 'px" ',
            '>', text, '</text>\n', file=file, append=TRUE)
}
