# brocolors
#' Vectors of colors for figures
#'
#' Creates different vectors of related colors that may be useful for figures.
#'
#' @param set Character string indicating a set of colors.
#' @return Vector of character strings representing the chosen set of colors, in RGB.
#' @export
#' @author Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#' @seealso \code{\link{plot_crayons}}
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
function(set=c("general", "bg", "bgpng", "CC", "f2", "sex", "main", "crayons"))
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

  crayons = c("Almond"="#efdecd",
              "Antique Brass"="#cd9575",
              "Apricot"="#fdd9b5",
              "Aquamarine"="#78dbe2",
              "Asparagus"="#87a96b",
              "Atomic Tangerine"="#ffa474",
              "Banana Mania"="#fae7b5",
              "Beaver"="#9f8170",
              "Bittersweet"="#fd7c6e",
              "Black"="#000000",
              "Blizzard Blue"="#ace5ee",
              "Blue"="#1f75fe",
              "Blue Bell"="#a2a2d0",
              "Blue Gray"="#6699cc",
              "Blue Green"="#0d98ba",
              "Blue Violet"="#7366bd",
              "Blush"="#de5d83",
              "Brick Red"="#cb4154",
              "Brown"="#b4674d",
              "Burnt Orange"="#ff7f49",
              "Burnt Sienna"="#ea7e5d",
              "Cadet Blue"="#b0b7c6",
              "Canary"="#ffff99",
              "Caribbean Green"="#00CC99",
              "Carnation Pink"="#ffaacc",
              "Cerise"="#dd4492",
              "Cerulean"="#1dacd6",
              "Chestnut"="#bc5d58",
              "Copper"="#dd9475",
              "Cornflower"="#9aceeb",
              "Cotton Candy"="#ffbcd9",
              "Dandelion"="#fddb6d",
              "Denim"="#2b6cc4",
              "Desert Sand"="#efcdb8",
              "Eggplant"="#6e5160",
              "Electric Lime"="#ceff1d",
              "Fern"="#71bc78",
              "Forest Green"="#6dae81",
              "Fuchsia"="#c364c5",
              "Fuzzy Wuzzy"="#cc6666",
              "Gold"="#e7c697",
              "Goldenrod"="#fcd975",
              "Granny Smith Apple"="#a8e4a0",
              "Gray"="#95918c",
              "Green"="#1cac78",
              "Green Blue"="#1164b4",
              "Green Yellow"="#f0e891",
              "Hot Magenta"="#ff1dce",
              "Inchworm"="#b2ec5d",
              "Indigo"="#5d76cb",
              "Jazzberry Jam"="#ca3767",
              "Jungle Green"="#3bb08f",
              "Laser Lemon"="#fefe22",
              "Lavender"="#fcb4d5",
              "Lemon Yellow"="#fff44f",
              "Macaroni and Cheese"="#ffbd88",
              "Magenta"="#f664af",
              "Magic Mint"="#aaf0d1",
              "Mahogany"="#cd4a4c",
              "Maize"="#edd19c",
              "Manatee"="#979aaa",
              "Mango Tango"="#ff8243",
              "Maroon"="#c8385a",
              "Mauvelous"="#ef98aa",
              "Melon"="#fdbcb4",
              "Midnight Blue"="#1a4876",
              "Mountain Meadow"="#30ba8f",
              "Mulberry"="#c54b8c",
              "Navy Blue"="#1974d2",
              "Neon Carrot"="#ffa343",
              "Olive Green"="#bab86c",
              "Orange"="#ff7538",
              "Orange Red"="#ff2b2b",
              "Orange Yellow"="#f8d568",
              "Orchid"="#e6a8d7",
              "Outer Space"="#414a4c",
              "Outrageous Orange"="#ff6e4a",
              "Pacific Blue"="#1ca9c9",
              "Peach"="#ffcfab",
              "Periwinkle"="#c5d0e6",
              "Piggy Pink"="#fddde6",
              "Pine Green"="#158078",
              "Pink Flamingo"="#fc74fd",
              "Pink Sherbert"="#f78fa7",
              "Plum"="#8e4585",
              "Purple Heart"="#7442c8",
              "Purple Mountain's Majesty"="#9d81ba",
              "Purple Pizzazz"="#fe4eda",
              "Radical Red"="#ff496c",
              "Raw Sienna"="#d68a59",
              "Raw Umber"="#714b23",
              "Razzle Dazzle Rose"="#ff48d0",
              "Razzmatazz"="#e3256b",
              "Red"="#ee204d",
              "Red Orange"="#ff5349",
              "Red Violet"="#c0448f",
              "Robin's Egg Blue"="#1fcecb",
              "Royal Purple"="#7851a9",
              "Salmon"="#ff9baa",
              "Scarlet"="#fc2847",
              "Screamin' Green"="#76ff7a",
              "Sea Green"="#93dfb8",
              "Sepia"="#a5694f",
              "Shadow"="#8a795d",
              "Shamrock"="#45cea2",
              "Shocking Pink"="#fb7efd",
              "Silver"="#cdc5c2",
              "Sky Blue"="#80daeb",
              "Spring Green"="#eceabe",
              "Sunglow"="#ffcf48",
              "Sunset Orange"="#fd5e53",
              "Tan"="#faa76c",
              "Teal Blue"="#18a7b5",
              "Thistle"="#ebc7df",
              "Tickle Me Pink"="#fc89ac",
              "Timberwolf"="#dbd7d2",
              "Tropical Rain Forest"="#17806d",
              "Tumbleweed"="#deaa88",
              "Turquoise Blue"="#77dde7",
              "Unmellow Yellow"="#ffff66",
              "Violet (Purple)"="#926eae",
              "Violet Blue"="#324ab2",
              "Violet Red"="#f75394",
              "Vivid Tangerine"="#ffa089",
              "Vivid Violet"="#8f509d",
              "White"="#FFFFFF",
              "Wild Blue Yonder"="#a2add0",
              "Wild Strawberry"="#ff43a4",
              "Wild Watermelon"="#fc6c85",
              "Wisteria"="#cda4de",
              "Yellow"="#fce883",
              "Yellow Green"="#c5e384",
              "Yellow Orange"="#ffb653")

  switch(match.arg(set),
         general=general,
         bg=bg,
         bgpng=bgpng,
         CC=CC,
         f2=f2,
         sex=sex,
         main=main,
         crayons=crayons)
}


#' Illustraion of crayon colors
#'
#' Creates a plot of the crayon colors in \code{\link{brocolors}}
#' @param cex character expansion for the text
#' @return None
#' @export
#' @author Karl W Broman, \email{kbroman@@biostat.wisc.edu}
#' @references \url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @seealso \code{\link{brocolors}}
#' @examples
#' plot_crayons()
plot_crayons <-
function(cex=0.6)
{
  col <- brocolors("crayons")

  # get rgb 
  colval <- t(col2rgb(col))

  # hclust to order the colors
  ord <- hclust(dist(colval))$order

  par(mar=rep(0.1,4))
  x <- (1:7)-1
  y <- (1:19)-1
  x <- rep(x, each=length(y))
  y <- rep(y, length(x))

  plot(0, 0, type="n", xlab="", ylab="", xaxs="i", yaxs="i",
       xlim=c(0, max(x)+1), ylim=c(max(y)+0.5, -0.5),
       xaxt="n", yaxt="n")
  
  dx <- 0.2
  dy <- 0.4
  rect(x+dx/4, y-dy, x+dx, y+dy, border="black",
       col=col[ord])
  text(x+dx*1.2, y, names(col)[ord], cex=cex, adj=c(0, 0.5))
}
