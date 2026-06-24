#!/usr/bin/Rscript

args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) >= 1)
file <- args[1]

x <- readLines(file)
badges <- readLines("badges.html")

wh <- grep("paste badges here", x)
if(length(wh) == 1) {
    x <- c(x[1:(wh-1)], badges, x[(wh+1):length(x)])
}

cat(x, file=file, sep="\n")
