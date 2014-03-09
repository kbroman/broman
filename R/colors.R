# brocolors
#' Vectors of colors for figures
#'
#' Creates different vectors of related colors that may be useful for figures.
#'
#' @param set Character string indicating a set of colors.
#' @return Vector of character strings representing the chosen set of colors, in RGB.
#' @export
#' @author Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#' @examples
#' col <- brocolors("general")
#' plot(seq(along=col), pch=21, bg=col, cex=4)
#' @keywords utilities
brocolors <-
function(set=c("general", "bg", "bgpng", "CC", "f2", "sex", "main"))
{
  general <- c('lightblue'  =rgb(102,203,254,maxColorValue=255),
               'hotpink'    =rgb(254,  0,128,maxColorValue=255),
               'pink'       =rgb(254,102,254,maxColorValue=255),
               'green'      =rgb(102,254,102,maxColorValue=255),
               'purple'     =rgb(128,  0,128,maxColorValue=255),
               'lightpurple'=rgb(203,102,254,maxColorValue=255),
               'yellow'     =rgb(254,203,102,maxColorValue=255),
               'darkblue'   =rgb(  0,128,128,maxColorValue=255))

  bg <- rgb(0, 0, 80, maxColorValue=255)
  bgpng <- rgb(0, 0, 98, maxColorValue=255)

  # text
  text <- c('yellow'   =rgb(255, 255, 102, maxColorValue=255),
            'lightblue'=rgb(102, 204, 255, maxColorValue=255),
             'pink'    =rgb(255, 102, 255, maxColorValue=255))

  CC <- c("AJ"  =rgb(240,240,  0,maxColorValue=255),
          "B6"  =rgb(128,128,128,maxColorValue=255),
          "129" =rgb(240,128,128,maxColorValue=255),
          "NOD" =rgb( 16, 16,240,maxColorValue=255),
          "NZO" =rgb(  0,160,240,maxColorValue=255),
          "CAST"=rgb(  0,160,  0,maxColorValue=255),
          "PWK" =rgb(240,  0,  0,maxColorValue=255),
          "WSB" =rgb(144,  0,224,maxColorValue=255))

  f2 <- c(AA=as.character(CC[1]), AB=rgb(0, 200, 0, maxColorValue=255), BB=as.character(CC[5]))
  sex <- c(female=rgb(255,80,80, maxColorValue=255), male=as.character(CC[5]))

  main <- rgb(0, 64, 128, maxColorValue=255)

  switch(match.arg(set),
         general=general,
         bg=bg,
         bgpng=bgpng,
         CC=CC,
         f2=f2,
         sex=sex,
         main=main)
}
