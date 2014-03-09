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
#' plot(0, 0, type="n", xlab="", ylab="", xlim=c(0, 9), ylim=c(7.5, 0), yaxs="i",
#'      xaxt="n", yaxt="n", mar=c(0.6, 5.1, 0.6, 0.6), xaxs="i")
#' axis(side=2, at=1:7, c("general", "bg", "bgpng", "CC", "f2", "sex", "main"), las=1)
#'
#' gen <- brocolors("general")
#' points(seq(along=gen), rep(1,length(gen)), pch=21, bg=gen, cex=4)
#' text(seq(along=gen), rep(c(0.55, 0.7), length(gen))[seq(along=gen)], names(gen))
#'
#' points(1, 2, pch=21, bg=brocolors("bg"), cex=4)
#' points(1, 3, pch=21, bg=brocolors("bgpng"), cex=4)
#'
#' CC <- brocolors("CC")
#' points(seq(along=CC), rep(4,length(CC)), pch=21, bg=CC, cex=4)
#' text(seq(along=CC), rep(3+c(0.55, 0.7), length(CC))[seq(along=CC)], names(CC))
#' 
#' f2 <- brocolors("f2")
#' points(seq(along=f2), rep(5,length(f2)), pch=21, bg=f2, cex=4)
#' text(seq(along=f2), rep(4.7, length(f2)), names(f2))
#' 
#' sex <- brocolors("sex")
#' points(seq(along=sex), rep(6,length(sex)), pch=21, bg=sex, cex=4)
#' text(seq(along=sex), rep(5.7, length(sex)), names(sex))
#' 
#' points(1, 7, pch=21, bg=brocolors("main"), cex=4)

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
