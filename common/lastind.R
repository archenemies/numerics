# FHE 25 Mar 2025
# https://stackoverflow.com/questions/44099454/in-r-how-to-index-only-the-last-dimension-of-an-array-when-you-dont-know-how-m/44099739#44099739
# subscript the last index of an array

lastInd <- function(x, n){
    nd <- length(dim(x))
    # uncomment the following line if x could be a plain vector without dim
    # FHE 05 Apr 2025 (why was it ever commented?)
    if(nd == 0) nd = 1
    inds <- rep(alist(,)[1], nd)
    inds[nd] <- n
    do.call(`[`, c(list(x), inds))
}

## lastInd <- function(x, n){
##     d <- dim(x)
##     if(is.null(d)){
##         x[n]
##     }else{
##         d.new <- d[-length(d)]
##         block.size <- prod(d.new)
##         res <- x[(block.size * (n - 1) + 1):(block.size * n)]
##         array(res, dim = d.new)
##     }
## }

# my addition
lastDim <- function(x) {
  d <- dim(x)
  if(is.null(d)){
    length(d)
  } else {
    tail(d,1)
  }
}

# helper function to append 1 to dimension list
vector_of = function(x, n=1) {
  v = rep(x, n)
  dim(v) = c(dim(x),n)
  v
}
