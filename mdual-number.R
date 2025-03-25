# FHE 24 Mar 2025
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0

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

if(1) {
# tests:
x = mdual_number(1, array(1), "dt")
promote_mdual(x, c("du","dt"))

arr = array(1:24, c(2,3,4))
ax = mdual_number(arr, vector_of(arr), "dt")
promote_mdual(ax, c("dt","du"))
}

# - sync_mduals
#   - input: list of mdual_number's.
#     output: same list but promoted to have same names
#   - call from op method
# \- rewrite to make $names the last dimension (rather than first) in $mdual
# \- in promote_mdual
#   \- use match() to get indices of new names
#   \- turn $mdual into matrix by setting dim()
#     /- need helper function for this?
#   \- use cbind to add zeroes, then the missing variables just refer to this zero column
#   \- we can then reindex like this > m[1:2,c(2,3,1,3)]
# - fill in the rest from dual-number.R

## # - use slice.index(x, 1) to get indices into $names
## #     and use slice.index(x, c(2,3)) to get indices into $value
