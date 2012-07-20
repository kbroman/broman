######################################################################
# convert a number to hex notation (and back)
#
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License,
#     version 3, as published by the Free Software Foundation.
#
#     This program is distributed in the hope that it will be useful,
#     but without any warranty; without even the implied warranty of
#     merchantability or fitness for a particular purpose.  See the GNU
#     General Public License, version 3, for more details.
#
#     A copy of the GNU General Public License, version 3, is available
#     at http://www.r-project.org/Licenses/GPL-3
#
######################################################################

dec2hex <-
convert2hex <-
function(d)
{
  if(length(d) > 1) {
    res <- d
    for(i in seq(along=d))
      res[i] <- convert2hex(d[i])
    return(res)
  }

  if(d != round(d) || d < 0)
    stop("d must be a non-negative integer")

  hex <- c(0:9,LETTERS[1:6])

  if(d < 16) return(hex[d+1])

  high <- floor(log(d, 16))

  res <- ""
  for(i in high:1) {
    z <- d %/% (16^i)
    res <- paste(res, hex[z + 1], sep="")
    d <- d - z*(16^i)
  }

  paste(res, hex[(d %% 16) + 1], sep="")
}

hex2dec <-
function(h)
{
  if(length(h) > 1) {
    res <- h
    for(i in seq(along=h))
      res[i] <- hex2dec(h[i])
    return(res)
  }

  hex <- c(0:9,LETTERS[1:6])

  if(!is.character(h)) h <- as.character(h)

  hspl <- rev(unlist(strsplit(h, "")))
  hc <- match(hspl, hex)-1
  if(any(is.na(hc)))
     stop("Invalid characters (", paste(hspl[is.na(hc)], collapse=" "), ") in ", h)

  sum(hc * 16^(0:(length(hc)-1)))
}

# end of convert2hex.R
