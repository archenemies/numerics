# Helpers:
# ----------------------------------------------------------------
# Define a special class "bbound" to manage block-bound objects,
# ensuring proper printing and attribute handling.

# Add class "bbound" to objects from bbind
as.bbound = function(x) {
  class(x) = c("bbound", class(x))
  x
}

# Print method for bbound class
print.bbound = function(x, ...) {
  # Temporarily hide dimblocks
  db = dimblocks(x)
  dimblocks(x) = NULL
  NextMethod()
  cat(sprintf("<dimblocks: %d dimensions with %d named blocks>\n",
    length(db),
    sum(sapply(db, function(d) if(is.null(d)) 0 else length(d))))
  )
}
# ----------------------------------------------------------------

# Attribute accessors for dimblocks
dimblocks = function(obj) {
  attr(obj, "dimblocks")
}
`dimblocks<-` = function(obj, ..., value) {
  attr(obj, "dimblocks") = value
  if(is.null(value)) {
    obj
  } else {
    as.bbound(obj)
  }
}

# ----------------------------------------------------------------
# Main functions:

# bprep "block prepare"
# Sets "dimblocks" attribute from dimnames(), creating block structures
# for each dimension with indices, names, and empty subblocks.
bprep = function(obj) {
  if(is.null(dimblocks(obj))) {
    dn = dimnames(obj)
    d = dim(obj)
    if(is.null(d)) d = length(obj)
    if(length(d) == 1) dn = list(names(obj))
    blocks = vector("list", length(d))
    for (i in seq_along(d)) {
      if (!is.null(dn[[i]])) {
        blocks[[i]] = list(
          default = list(
            ixs = seq_len(d[[i]]),
            names = dn[[i]],
            subblocks = NULL
          )
        )
      } else {
        blocks[[i]] = NULL
      }
    }
    dimblocks(obj) = blocks
  }
  obj
}

# bbind "block bind"
# Combine several block-prepared objects into a single bbound object
# along a specified dimension. Ensures non-binding dimensions and their
# dimnames match. Sets "dimblocks" attribute as a list of block lists,
# where each block contains indices, names, and subblocks.
# Example: u = bbind(dim=1, a=v1, b=v2)
# - dimblocks(u)[[1]] is list(a=list(ixs=..., names=..., subblocks=...), b=...)
# - dimnames(u)[[1]] is c("a$x", "a$y", ..., "b$u", "b$v", ...)
bbind = function(dim=NULL, ...) {
  args = list(...)
  if (is.null(dim)) stop("dimension must be specified")
  if (is.null(names(args)) || any(names(args) == "")) stop("all arguments must be named")

  args = lapply(args, bprep)

  ref_dims = dim(args[[1]])
  n_dims = length(ref_dims)
  if (dim > n_dims) stop("specified dim exceeds number of dimensions")

  # Validate dimensions and dimnames
  ref_dn = dimnames(args[[1]])
  for (a in args) {
    da = dim(a)
    if (length(da) != n_dims) stop("all arguments must have same number of dimensions")
    if (!all(da[-dim] == ref_dims[-dim])) stop("non-binding dimensions must match")
    dn = dimnames(a)
    for (i in seq_len(n_dims)) {
      if (i != dim && !is.null(ref_dn[[i]]) && !is.null(dn[[i]])) {
        if (!identical(ref_dn[[i]], dn[[i]])) stop("dimnames must match in non-binding dimensions")
      }
    }
  }

  # Calculate final dimension sizes
  bind_sizes = sapply(args, function(x) dim(x)[dim])
  out_dims = ref_dims
  out_dims[dim] = sum(bind_sizes)

  # Allocate result
  first = args[[1]]
  out = array(NA, dim=out_dims)
  storage.mode(out) = storage.mode(first)

  # Fill result
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

  # Handle dimnames
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

  # dimblocks
  blocks = vector("list", n_dims)
  for (d in seq_len(n_dims)) {
    if (d == dim) {
      blocks[[d]] = list()
      start = 1
      for (nm in names(args)) {
        obj = args[[nm]]
        len = dim(obj)[d]
        obj_blocks = dimblocks(obj)[[d]]
        subblocks = if (!is.null(obj_blocks)) obj_blocks$default$subblocks else NULL
        blocks[[d]][[nm]] = list(
          ixs = start:(start + len - 1),
          names = dimnames(obj)[[d]],
          subblocks = subblocks
        )
        start = start + len
      }
    } else {
      blocks[[d]] = dimblocks(first)[[d]]
    }
  }
  dimblocks(out) = blocks
  out
}

# bfetch "block fetch"
# Retrieves block data (indices, names, subblocks) for a given path
# (e.g., "a", "a$x") along a specified dimension. Splits path on "$"
# to navigate the block hierarchy.
bfetch = function(obj, path, dim=NULL) {
  blocks = dimblocks(obj)
  if (is.null(blocks)) stop("no dimblocks found")
  
  # Determine dimension if not specified
  if (is.null(dim)) {
    found = which(sapply(blocks, function(b) !is.null(b)))
    if (length(found) == 0) stop("no blocks found")
    if (length(found) > 1) stop("dimension must be specified when multiple dimensions have blocks")
    dim = found
  }
  
  if (is.null(blocks[[dim]])) stop("no blocks found for dimension ", dim)
  
  # Split path on "$"
  path_parts = strsplit(path, "\\$")[[1]]
  if (length(path_parts) == 0) stop("invalid path")
  
  # Navigate block hierarchy
  current = blocks[[dim]]
  result = NULL
  for (i in seq_along(path_parts)) {
    part = path_parts[i]
    if (!part %in% names(current)) stop("block not found: ", path)
    if (i == length(path_parts)) {
      result = current[[part]]
      break
    }
    current = current[[part]]$subblocks
    if (is.null(current)) stop("no subblocks for path: ", path)
  }
  
  if (is.null(result)) stop("block not found: ", path)
  result
}

# Subscript method for bbound objects
# Handles indexing with block names or paths (e.g., u["a",], u["a$x",])
# Uses bfetch to retrieve block data and returns a bbound object
# with correct dimblocks and dimnames.
`[.bbound` = function(x, ...) {
  args = list(...)
  n_dims = length(dim(x))
  if (length(args) > n_dims) stop("too many indices")
  
  # Identify dimension with character index (block name/path)
  char_idx = which(sapply(args, is.character))
  if (length(char_idx) > 1) stop("only one dimension can be indexed by block name")
  
  if (length(char_idx) == 0) {
    # Standard numeric indexing, preserve bbound class
    result = NextMethod()
    if (!is.array(result)) return(result)
    db = dimblocks(x)
    if (!is.null(db)) {
      new_db = vector("list", length(dim(result)))
      for (i in seq_along(new_db)) {
        if (i <= length(db) && !is.null(db[[i]])) {
          # Simplify blocks for subsetted dimensions
          new_db[[i]] = db[[i]]
        }
      }
      dimblocks(result) = new_db
    }
    return(as.bbound(result))
  }
  
  dim = char_idx
  path = args[[dim]]
  if (length(path) > 1) stop("only one block path allowed")
  
  # Fetch block data
  block = bfetch(x, path, dim)
  idx = vector("list", n_dims)
  for (i in seq_len(n_dims)) {
    if (i == dim) {
      idx[[i]] = block$ixs
    } else {
      idx[[i]] = if (is.null(args[[i]])) seq_len(dim(x)[i]) else args[[i]]
    }
  }
  
  # Extract subset
  result = do.call(`[`, c(list(x), idx, list(drop=FALSE)))
  
  # Update dimblocks and dimnames
  if (is.array(result)) {
    db = dimblocks(x)
    new_db = vector("list", length(dim(result)))
    for (i in seq_along(new_db)) {
      if (i == dim && !is.null(block$subblocks)) {
        new_db[[i]] = block$subblocks
      } else if (i <= length(db)) {
        new_db[[i]] = db[[i]]
      }
    }
    dimblocks(result) = new_db
    
    # Update dimnames
    dn = dimnames(x)
    if (!is.null(dn)) {
      new_dn = vector("list", length(dim(result)))
      for (i in seq_along(new_dn)) {
        if (i == dim && !is.null(block$names)) {
          new_dn[[i]] = block$names
        } else if (i <= length(dn)) {
          new_dn[[i]] = dn[[i]]
        }
      }
      dimnames(result) = new_dn
    }
    
    result = as.bbound(result)
  }
  
  result
}

# Assignment method for bbound objects
# Handles assignment to block-indexed subsets (e.g., u["a",] <- value)
`[<-.bbound` = function(x, ..., value) {
  args = list(...)
  n_dims = length(dim(x))
  if (length(args) > n_dims) stop("too many indices")
  
  char_idx = which(sapply(args, is.character))
  if (length(char_idx) > 1) stop("only one dimension can be indexed by block name")
  
  if (length(char_idx) == 0) {
    # Standard numeric indexing
    result = NextMethod()
    return(as.bbound(result))
  }
  
  dim = char_idx
  path = args[[dim]]
  if (length(path) > 1) stop("only one block path allowed")
  
  # Fetch block data
  block = bfetch(x, path, dim)
  idx = vector("list", n_dims)
  for (i in seq_len(n_dims)) {
    if (i == dim) {
      idx[[i]] = block$ixs
    } else {
      idx[[i]] = if (is.null(args[[i]])) seq_len(dim(x)[i]) else args[[i]]
    }
  }
  
  # Perform assignment
  x = do.call(`[<-`, c(list(x), idx, list(value=value)))
  
  # Preserve dimblocks
  if (is.array(x)) {
    dimblocks(x) = dimblocks(x)
    x = as.bbound(x)
  }
  
  x
}