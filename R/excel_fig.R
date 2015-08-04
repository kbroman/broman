#' Excel-style figure displaying contents of a matrix
#'
#' Turn a matrix of data into an SVG of how it might look in Excel
#'
#' @param mat A matrix
#' @param file Optional file name (must have extension .svg, .png, .jpg, or .pdf)
#' @param cellwidth Width of each cell, in pixels
#' @param cellheight Height of each cell, in pixels
#' @param pointsize Pointsize for text (if \code{file} is provided)
#' @param border Color of border of cells for the body of the matrix
#' @param headcol Background color of cells on the top and left border
#' @param headborder Color of border of cells on the top and left border
#' @param row_names If TRUE, and row names are present, include them as a first column
#' @param col_names If TRUE, and column names are present, include them as a first row
#' @param hilitcells Character vector of cells to highlight, like \code{"A1"} or \code{"D4"}
#' @param hilitcolor Color to highlight cells, a vector of length 1 or the same length as \code{hilitcells}
#'
#' @export
#' @importFrom grDevices svg png jpeg pdf dev.off
#' @importFrom graphics plot rect text par
#'
#' @keywords hplot
#'
#' @examples
#' df <- data.frame(id=    c(101,  102,  103),
#'                  sex=   c("M",  "F",  "M"),
#'                  weight=c(22.3, 15.8, 19.7),
#'                  stringsAsFactors=FALSE)
#' excel_fig(df, col_names=TRUE)
excel_fig <-
    function(mat, file, cellwidth=100, cellheight=20, pointsize=14,
             border="#CECECE", headcol="#E9E9E9", headborder="#969696",
             row_names=FALSE, col_names=TRUE,
             hilitcells, hilitcolor="#F0DCDB")

{
    headtextcol <- "#626262"
    textcol <- "black"

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
    celly <- cumsum(c(0,cellheight))
    cellx <- cumsum(c(0,cellwidth))

    if(!missing(file)) {
        if(grepl("\\.svg$", file))
            svg(file, width=width/72, height=height/72, pointsize=pointsize)
        else if(grepl("\\.png$", file))
            png(file, width=width, height=height, pointsize=pointsize)
        else if(grepl("\\.pdf$", file))
            pdf(file, width=width/72, height=height/72, pointsize=pointsize)
        else if(grepl("\\.jpg$", file))
            jpeg(file, width=width, height=height, pointsize=pointsize)
        else
            stop("file must have extension .svg, .png, .jpg, or .pdf")
    }

    # plot region
    par(mar=rep(0.1, 4))
    plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
         xlim=c(0, width), ylim=c(height, 0), xaxs="i", yaxs="i")

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


    for(i in n_row:0) {
        for(j in n_col:0) {
            # rectangles
            rect(cellx[j+1], celly[i+1], cellx[j+2], celly[i+2],
                 border=ifelse(i==0 || j==0, headborder, border),
                 col=ifelse(i==0 || j==0, headcol, colormat[i,j]))

            # text
            if(i==0 && j>0)
                text(mean(cellx[j+1:2]), mean(celly[1:2]), LETTERS[j], col=headtextcol, font=2)
            if(i>0 && j==0)
                text(mean(cellx[1:2]), mean(celly[i+1:2]), i, col=headtextcol, font=2)
            if(i>0 && j>0)
                text(mean(cellx[j+1:2]), mean(celly[i+1:2]), mat[i,j], col=textcol)
        }
    }

    if(!missing(file))
        dev.off()
}
