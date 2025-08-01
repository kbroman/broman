#' Jiggle points horizontally
#'
#' Spread points out horizontally so that, in dot plot of quantitative
#' response in multiple categories, the separate points can be seen.
#'
#' @param group Categorical variable defining group; can be a factor,
#' character, or numeric vector
#'
#' @param y Vector of quantitative responses
#'
#' @param method What method to use for horizontal jiggling.
#'
#' @param hnum Number of horizontal bins for the jiggling.
#' @param vnum Number of vertical bins for the jiggling.
#'
#' @param maxvalue Maximum value in the results; results will be scaled to this value.
#' Use `NULL` to not scale.
#'
#' @details The `"random"` method is similar to
#' [base::jitter()] but with amount of jiggling proportional
#' to the number of nearby points. The `"fixed"` method is
#' similar to the [beeswarm package](https://github.com/aroneklund/beeswarm)
#'
#' @return Numeric vector with amounts to jiggle the points horizontally
#'
#' @seealso [base::jitter()], [dotplot()]
#' @importFrom stats runif median
#' @useDynLib broman, .registration=TRUE
#' @export
jiggle <-
    function(group, y, method=c("random", "fixed"), hnum=35, vnum=40, maxvalue=0.45)
{
    method <- match.arg(method)
    stopifnot(length(group) == length(y))

    # turn group into numbers 1, 2, ..., n_group
    if(is.factor(group)) {
        ugroup <- levels(factor(group)) # drop missing levels
        group <- as.numeric(group)
    }
    else {
        ugroup <- sort(unique(group))
        group <- match(group, ugroup)
    }
    n_group <- length(ugroup)

    if(length(unique(y)) < length(unique(group)))
        warning('Seems like maybe "group" and "y" got switched.')

    hamount <- n_group/hnum
    vamount <- diff(range(y, na.rm=TRUE))/vnum

    if(method=="random") {
        hamount <- ifelse(is.null(maxvalue), 0.45, maxvalue)
        yspl <- split(y, group)
        yspli <- split(seq(along=y), group)

        # for each value, count number of values in group that are within hamount
        nclose <- seq(along=y)
        for(i in 1:n_group) {
            nclose[yspli[[i]]] <-
                .C("R_count_close",
                   as.double(yspl[[i]]),
                   as.integer(length(yspl[[i]])),
                   as.double(vamount),
                   counts=as.integer(rep(0, length(yspl[[i]]))),
                   PACKAGE="broman")$counts
        }
        hamount <- nclose*hamount/max(c(nclose, 5))

        return(runif(length(y), -hamount, hamount))
    }
    else if(method=="fixed") {
        # breaks between intervals (intervals centered at median)
        br <- c(rev(seq(median(y, na.rm=TRUE)+vamount/2,
                        min(y, na.rm=TRUE)-vamount,
                        by=-vamount)),
                seq(median(y, na.rm=TRUE)+vamount/2,
                    max(y, na.rm=TRUE)+vamount,
                    by=vamount)[-1])
        n <- length(br)-1

        # midpoints; expand first and last intervals
        mid <- (br[-1] + br[-(n+1)])/2
        br[1] <- br[1]-1
        br[n+1] <- br[n+1] +1

        # coarsen the y values
        ycat <- cut(y, br)

        # split y values by group
        yspl <- lapply(split(ycat, group), function(a) as.numeric(a))
        yospl <- lapply(split(y, group), function(a) as.numeric(a))
        gspl <- split(group, group)
        indexspl <- split(seq(along=y), group)

        grev <- group
        for(i in seq(along=yspl)) {
            tab <- table(yspl[[i]])
            tabname <- as.numeric(names(tab))
            grp <- gspl[[i]][1]
            for(j in seq(along=tab)) {
                if(tab[j]>1) {
                    tmp <- seq(0, by=hamount, length=tab[j])
                    tmp <- tmp - mean(tmp) + grp

                    # create pattern: lowest values on outside; highest values in middle
                    thisy <- yospl[[i]][yspl[[i]]==tabname[j]]
                    o <- order(thisy)
                    if(length(o) %% 2) o <- c(o, NA)
                    o <- matrix(o, byrow=TRUE, ncol=2)
                    o[,2] <- rev(o[,2])
                    o <- as.numeric(o)
                    o <- o[!is.na(o)]

                    gspl[[i]][yspl[[i]]==tabname[j]] <- tmp[o]
                }
            }
            grev[indexspl[[i]]] <- gspl[[i]]
        }

        result <- grev-group
        attr(result, "breaks") <- br

        # rescale to be in the range -maxvalue to maxvalue
        if(!is.null(maxvalue)) {
            result <- result/max(abs(result))*maxvalue
        }

        return(result)
    }
}
