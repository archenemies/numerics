# FHE 24 Mar 2025
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0

mysource("lastind.R")

mdual_number <- function(value, mdual, names) {
  # Validate inputs
  stopifnot(is.numeric(value))
  stopifnot(is.numeric(mdual))
  stopifnot(is.character(names))

  ndim = length(dim(mdual))
  if (ndim < 1) {
    stop("mdual must have at least one dimension")
  }
  nd = dim(mdual)[ndim]
  if (nd != length(names)) {
    stop("Last dimension of mdual must match length of names")
  }
  if (any(duplicated(names))) {
    stop("names must be unique")
  }

  if(!identical(c(dim(value), nd), dim(mdual))) {
    stop("non-conforming dimensions for value and mdual: ",
      sv(dim(value)), "; ", sv(dim(mdual)))
  }

  # Create and return the object
  structure(list(value = value, mdual = mdual, names = names), class = "mdual_number")
}

is.mdual = function(x) { inherits(x, "mdual_number") }

stripClass = function(x) {
  class(x) = NULL
  x
}

format.mdual_number = function(x) {
  cat("mdual:\n")
  format(stripClass(x))
}

print.mdual_number = function(x) {
  ns = x$names
  cat("mdual_number: ", paste0(ns, collapse=","), "\n")
  cat("  VALUE=\n")
  print(x$value)
  if(0) {
    cat("  MDUAL=\n")
    print(x$mdual)
  } else {
    stopifnot(length(ns) == lastDim(x$mdual))
    for(i in seq_along(ns)) {
      cat("  DUAL[",i,"]= (",ns[i],")\n",sep="")
      print(lastInd(x$mdual, i))
    }
  }
}

# helper function to append 1 to dimension list
vector_of = function(x) {
  dim(x) = c(dim(x),1)
  x
}

# return mdual_number with new_names for names
promote_mdual = function(mx, new_names) {
  if(identical(new_names, mx$names)) return(mx);
  if(!all(mx$names %in% new_names)) {
    stop("new_names must be a superset of mx$names: ",
      sv(mx$names), " ", sv(new_names))
  }

  mxd = mx$mdual
  old_dims = dim(mxd)
  ndim = length(old_dims);
  stopifnot(ndim>=1)
  nd = old_dims[ndim]
  stopifnot(nd==length(mx$names))
  nnd = length(new_names)

  # turn argument mdual into a matrix, temporarily
  mat_dims = c(prod(old_dims[-ndim]), nd)
  dim(mxd) = mat_dims;
  # add a column of zeros
  mxd = cbind(mxd, 0)
  # now the last dim() is incremented by 1
  stopifnot(dim(mxd)[2] == (nd+1))
  new_col_ids = match(mx$names, new_names)
  col_sources = rep(nd+1, length(new_names))
  col_sources[new_col_ids] = 1 %upto% length(mx$names)
  nr = nrow(mxd)
  # rearrange the columns to match new_names
  mxd = mxd[1 %upto% nr, col_sources, drop=F]
  new_dims = old_dims;
  new_dims[ndim] = nnd
  dim(mxd) = new_dims

  mx$mdual = mxd
  mx$names = new_names
  mx
}

# sync_mduals: promote each mdual_number the list mds
# input: list of mdual_numbers
# output: same list but promoted to have same $names
sync_mduals = function(mds) {
  stopifnot(is.list(mds))
  all_names=list()
  for(md in mds) {
    all_names = union(all_names, md$names)
  }
  # (for some reason union makes it a list:)
  all_names = as.character(all_names)
  res_mds = list()
  for(i in seq_along(mds)) {
    res_mds[[i]] = promote_mdual(mds[[i]], all_names)
  }
  res_mds
}


if(1) {
  # tests:
  x = mdual_number(1, array(1), "dt")
  promote_mdual(x, c("du","dt"))

  arr = array(1:24, c(2,3,4))
  ax = mdual_number(arr, vector_of(arr), "dt")
  promote_mdual(ax, c("dt","du"))

  y = mdual_number(4, array(1), "du")
  s = sync_mduals(list(x,y))
}

# - call sync_mduals from op method
# - fill in the rest from dual-number.R
