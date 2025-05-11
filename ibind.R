# FHE 11 May 2025
# with help from ChatGPT

# ibind "index bind"
# Combine several indexed named objects into an indexed named object
# Requires inputs to be indexed
# u = ibind(dim=1, a=v, b=abs(v))
# - the argument objects should have all dimensions equal except for
# the binding dimension (and dimnames equal, if set)
# - sets "dimindices" attribute in addition to "dimnames"
# - dimindices(u) is a list of lists, analogous to dimnames
#   - the outer list is the same length as dim(u)
#   - each inner list, if not NULL, maps character index keys to integer index vectors
# - ibind uses (and requires) "dimindices" attributes in arguments, where names exist
# - for example if v has an element name "x" then it will be named "a$x" in the output, dimindices(u)[[1]][["a"]] will be 1 %upto% length(v), dimindices(u)[[1]][["a$x"]] will be the same as dimindices(v)[[1]][["x"]], a single integer indexing the "x" element of v; dimindices(u)[[1]][["b"]] will be (length(v)+1) %upto% (length(v)+length(v)), and so on.
# - if v is not named, then u will have names like "a$1", "a$2", "b$1", etc. These numerical names don't have to be in the index for u, since the user could just call ifetch(u, "a")[[2]] instead of ifetch(u, "a$2").
# - we require all ... arguments to be named
ibind = function(dim=NULL, ...) {
  args = list(...)
  if (is.null(dim)) stop("dimension must be specified")
  if (is.null(names(args)) || any(names(args) == "")) stop("all arguments must be named")

  args = lapply(args, iprep)

  ref_dims = dim(args[[1]])
  n_dims = length(ref_dims)
  if (dim > n_dims) stop("specified dim exceeds number of dimensions")

  for (a in args) {
    da = dim(a)
    if (length(da) != n_dims) stop("all arguments must have same number of dimensions")
    if (!all(da[-dim] == ref_dims[-dim])) stop("non-binding dimensions must match")
  }

  # calculate final dimension sizes
  bind_sizes = sapply(args, function(x) dim(x)[dim])
  out_dims = ref_dims
  out_dims[dim] = sum(bind_sizes)

  # allocate result
  first = args[[1]]
  out = array(NA, dim=out_dims)
  storage.mode(out) = storage.mode(first)

  # fill result
  start = 1
  for (i in seq_along(args)) {
    a = args[[i]]
    len = dim(a)[dim]
    end = start + len - 1
    idx = vector("list", n_dims)
    for (j in seq_len(n_dims)) {
      if (j == dim) {
        idx[[j]] = start:end
      } else {
        idx[[j]] = seq_len(dim(a)[j])
      }
    }
    out_idx = idx
    out_idx[[dim]] = start:end
    out = do.call(`[<-`, c(list(out), out_idx, list(value=a)))
    start = end + 1
  }

  # handle dimnames
  dnames = vector("list", n_dims)
  if (!is.null(dimnames(first))) {
    for (d in seq_len(n_dims)) {
      if (d == dim) {
        dnames[[d]] = unlist(Map(function(name, x) {
          dn = dimnames(x)[[d]]
          if (is.null(dn)) paste0(name, "$", seq_len(dim(x)[d]))
          else paste0(name, "$", dn)
        }, names(args), args), use.names=FALSE)
      } else {
        dnames[[d]] = dimnames(first)[[d]]
      }
    }
  }
  dimnames(out) = dnames

  # dimindices
  indices = vector("list", n_dims)
  for (d in seq_len(n_dims)) {
    if (d == dim) {
      indices[[d]] = list()
      start = 1
      for (nm in names(args)) {
        obj = args[[nm]]
        len = dim(obj)[d]
        indices[[d]][[nm]] = start %upto% (start + len - 1)
        obj_indices = attr(obj, "dimindices")[[d]]
        if (!is.null(obj_indices)) {
          for (subnm in names(obj_indices)) {
            indices[[d]][[paste0(nm, "$", subnm)]] = start - 1 + obj_indices[[subnm]]
          }
        }
        start = start + len
      }
    } else {
      indices[[d]] = attr(first, "dimindices")[[d]]
    }
  }
  attr(out, "dimindices") = indices
  out
}

# iprep "prepare index"
# Sets "dimindices" attribute from dimnames()
iprep = function(obj) {
  dn = dimnames(obj)
  d = dim(obj)
  indices = vector("list", length(d))
  for (i in seq_along(d)) {
    if (!is.null(dn[[i]])) {
      indices[[i]] = setNames(as.list(seq_len(d[[i]])), dn[[i]])
    } else {
      indices[[i]] = NULL
    }
  }
  attr(obj, "dimindices") = indices
  obj
}

# ifetch(u, "a", dim=1)
# ifetch(u, "b$x")
# Subscript the given object with the named indices along the specified dimension.
# Error if named indices don't exist.
ifetch = function(obj, ixkey, dim=NULL) {
  indices = attr(obj, "dimindices")
  if (is.null(dim)) {
    found = which(sapply(indices, function(idx) !is.null(idx) && ixkey %in% names(idx)))
    if (length(found) != 1) stop("ambiguous or missing dimension for index key")
    dim = found
  }
  if (is.null(indices[[dim]])) stop("no indices found for dimension ", dim)
  ix = indices[[dim]][[ixkey]]
  if (is.null(ix)) stop("index key not found: ", ixkey)
  idx = vector("list", length(dim(obj)))
  for (i in seq_along(idx)) idx[[i]] = seq_len(dim(obj)[i])
  idx[[dim]] = ix
  do.call(`[`, c(list(obj), idx, drop=FALSE))
}
