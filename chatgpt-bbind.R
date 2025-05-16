# Helpers:
# ----------------------------------------------------------------
# Add class "bbound" to objects from bbind
as.bbound = function(x) {
  class(x) = c("bbound", class(x))
  x
}

# Print method for bbound class
print.bbound = function(x, ...) {
  dbx = dimblocks(x)
  dimblocks(x) = NULL
  NextMethod()
  cat(sprintf("<dimblocks: %d dimensions with %d block groups>\n",
              length(dbx),
              sum(sapply(dbx, function(b) length(b)))))
}

# dimblocks getter and setter
dimblocks = function(obj) {
  attr(obj, "dimblocks")
}
`dimblocks<-` = function(obj, ..., value) {
  attr(obj, "dimblocks") = value
  if (is.null(value)) obj else as.bbound(obj)
}

# ----------------------------------------------------------------
# Main functions:

# bprep: prepare block structure based on dimnames
bprep = function(obj) {
  if (is.null(dimblocks(obj))) {
    dn = dimnames(obj)
    d = dim(obj)
    blocks = vector("list", length(d))
    for (i in seq_along(d)) {
      if (!is.null(dn[[i]])) {
        blocks[[i]] = setNames(lapply(seq_len(d[[i]]), function(j) {
          list(ixs = j, names = dn[[i]][j], subblocks = NULL)
        }), dn[[i]])
      } else {
        blocks[[i]] = NULL
      }
    }
    dimblocks(obj) = blocks
  }
  obj
}

# bbind: block-bind multiple bbound objects along one dimension
bbind = function(dim = NULL, ...) {
  args = list(...)
  if (is.null(dim)) stop("dimension must be specified")
  if (is.null(names(args)) || any(names(args) == "")) stop("all arguments must be named")

  args = lapply(args, bprep)
  ref_dims = dim(args[[1]])
  n_dims = length(ref_dims)
  if (dim > n_dims) stop("specified dim exceeds number of dimensions")

  for (a in args) {
    if (length(dim(a)) != n_dims) stop("dimension mismatch")
    if (!all(dim(a)[-dim] == ref_dims[-dim])) stop("non-binding dimensions must match")
  }

  # allocate result
  bind_sizes = sapply(args, function(x) dim(x)[dim])
  out_dims = ref_dims
  out_dims[dim] = sum(bind_sizes)
  first = args[[1]]
  out = array(NA, dim = out_dims)
  storage.mode(out) = storage.mode(first)

  # fill and track block info
  out_blocks = vector("list", n_dims)
  for (d in seq_len(n_dims)) {
    out_blocks[[d]] = if (d == dim) list() else dimblocks(first)[[d]]
  }

  start = 1
  for (nm in names(args)) {
    a = args[[nm]]
    len = dim(a)[dim]
    end = start + len - 1

    idx = rep(list(quote(expr=)), n_dims)
    for (j in seq_len(n_dims)) {
      idx[[j]] = if (j == dim) start:end else seq_len(dim(a)[j])
    }
    out = do.call(`[<-`, c(list(out), idx, list(value = a)))

    # Copy blocks
    ablocks = dimblocks(a)[[dim]]
    mapped_blocks = lapply(ablocks, function(b) {
      b$ixs = b$ixs + (start - 1)
      b
    })
    out_blocks[[dim]][[nm]] = list(ixs = start:end, names = NULL, subblocks = mapped_blocks)

    start = end + 1
  }

  dimnames(out) = {
    dnames = vector("list", n_dims)
    for (d in seq_len(n_dims)) {
      if (d == dim) {
        dnames[[d]] = unlist(Map(function(name, x) {
          dn = dimnames(x)[[d]]
          if (is.null(dn)) paste0(name, "$", seq_len(dim(x)[d]))
          else paste0(name, "$", dn)
        }, names(args), args), use.names = FALSE)
      } else {
        dnames[[d]] = dimnames(first)[[d]]
      }
    }
    dnames
  }

  dimblocks(out) = out_blocks
  out
}

# bfetch: fetch a block path from the block tree
bfetch = function(obj, path, dim) {
  parts = strsplit(path, "\\$", fixed = FALSE)[[1]]
  blocks = dimblocks(obj)[[dim]]
  for (p in parts) {
    if (is.null(blocks[[p]])) stop("Block not found: ", p)
    blk = blocks[[p]]
    blocks = blk$subblocks
  }
  blk
}

# [ method for bbound
`[.bbound` = function(x, i, j, ..., drop = TRUE) {
  subs = list(i, j, ...)
  dn = dim(x)
  nd = length(dn)
  idx = vector("list", nd)
  args = match.call()

  for (d in seq_len(nd)) {
    arg = subs[[d]]
    if (is.character(arg) && length(arg) == 1 && grepl("\\$", arg)) {
      blk = bfetch(x, arg, dim = d)
      idx[[d]] = blk$ixs
    } else {
      idx[[d]] = if (missing(subs[[d]])) seq_len(dim(x)[d]) else arg
    }
  }

  result = do.call(`[`, c(list(x), idx, list(drop = drop)))
  if (!drop) result = bprep(result)
  result
}

# ----------------------------------------------------------------
# Utility: like `:` but ensures result is an integer vector
`%upto%` = function(a, b) {
  seq.int(a, b)
}
