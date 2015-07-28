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
             row_names=FALSE, col_names=TRUE)

{
    headtextcol <- "#626262"
    textcol <- "black"

    if(row_names && !is.null(rownames(mat)))
        mat <- cbind("row"=row_names, mat, stringsAsFactors=FALSE)
    if(col_names && !is.null(colnames(mat)))
        mat <- rbind(colnames(mat), mat)

    n_row <- nrow(mat)
    n_col <- ncol(mat)
    height <- cellheight * (n_row + 1)
    width <- cellwidth * (n_col + 1)

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

    for(i in n_row:0) {
        for(j in n_col:0) {
            # rectangles
            rect(j*cellwidth, i*cellheight, (j+1)*cellwidth, (i+1)*cellheight,
                 border=ifelse(i==0 || j==0, headborder, border),
                 col=ifelse(i==0 || j==0, headcol, "white"))

            # text
            if(i==0 && j>0)
                text(cellwidth*(j+0.5), cellheight/2, LETTERS[j], col=headtextcol, font=2)
            if(i>0 && j==0)
                text(cellwidth/2, cellheight*(i+0.5), i, col=headtextcol, font=2)
            if(i>0 && j>0)
                text(cellwidth*(j+0.5), cellheight*(i+0.5), mat[i,j], col=textcol)
        }
    }

    if(!missing(file))
        dev.off()
}
