objectsizes <-
function(obj, sortbysize=TRUE)
{
  if(missing(obj)) obj <- objects(pos=1)
  result <- data.frame(Mb=rep(0, length(obj)))
  rownames(result) <- obj
  for(i in seq(along=obj))
    result[i,1] <- object.size(get(obj[i], pos=1))/1024^2
  if(sortbysize) result <- result[order(result[,1], decreasing=FALSE),,drop=FALSE]
  result
}
