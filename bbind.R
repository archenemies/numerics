# -*- my-source-delegate: "test-bbind.R" -*-
# FHE 18 May 2025
# Define bbind(), a function for managing blocks of indices in vector and array objects.
# From drafts produced by Grok and ChatGPT

# Adds attribute "dimblocks" to objects, similar to "dimnames"
# It is a list of same length as dim(x).
# The entries are the named block data for the vector.

# Each named block is stored as a list with names "ixs" (for the
# indices of the block), "names" (for the dimnames of the block), and
# "subblocks" (for any subblocks)

# Helpers:
# ----------------------------------------------------------------
# Add class "bbound" to objects from bbind
as.bbound = function(x) {
  class(x) = c("bbound", class(x))
  x
}

un_bbound = function(x) {
  class(x) = setdiff(class(x), "bbound")
  x
}

# Print method for bbound class
print.bbound = function(x, ...) {
  dbx = dimblocks(x)
  dimblocks(x) = NULL
  NextMethod()
  group_lens = vapply(dbx, function(b) length(b), 0)
  cat(sprintf("<dimblocks: %d dimensions with %s block groups>\n",
              length(dbx),
              paste0(group_lens, collapse=" ")))
}

# dimblocks getter and setter
dimblocks = function(obj) {
  attr(obj, "dimblocks")
}
`dimblocks<-` = function(obj, ..., value) {
  attr(obj, "dimblocks") = value
  res = if(is.null(value)) obj else as.bbound(obj)
  invisible(res)
}

# return dimnames of an object, or list(names(obj)) if names is set
dimnames_or_names = function(obj) {
  res = dimnames(obj)
  if(is.null(res)) {
    nas = names(obj);
    if(!is.null(nas)) {
      stopifnot(is.null(dim(obj)) || length(dim(obj))==1)
      list(nas)
    } else {
      NULL
    }
  } else {
    res
  }
}

# return dimensions for an object
# or length if unset
dim_or_length = function(obj) {
  res = dim(obj)
  if(is.null(res)) length(obj)
  else res
}

# ----------------------------------------------------------------
# main interface functions

# bprep: prepare block structure based on dimnames. promotes a vector
# or array to a bbound object.
bprep = function(obj) {
  if(is.null(dimblocks(obj))) {
    d = dim(obj)
    if(is.null(d)) d=length(obj)
    dn = dimnames_or_names(obj) # same length as d
    # new dimblocks
    dbs = vector("list", length(d))
    for (i in seq_along(d)) {
      if (!is.null(dn[[i]])) {
        # if we have names for a given dimension, then
        # these become the (length 1) blocks
        blck = lapply(seq_len(d[[i]]), function(j) {
          list(ixs = j, names = dn[[i]][j], subblocks = NULL)
        })
        names(blck) = dn[[i]]
        dbs[[i]] = blck
      } else {
        # otherwise, no blocks for that dimension
        dbs[i] = list(NULL)
      }
    }
    dimblocks(obj) = dbs
  } else {
    warning("bprep: called on object with non-NULL dimblocks")
  }
  obj
}

# bind multiple objects along a given dimension "along". the object
# dimensions must match in all other dimensions than "along".
# the arguments are numeric objects and must be named.
# e.g. bbind(a=v1, b=v2, .along=1)
# in no-names mode, none of the input objects are labeled, and they
# are required not to have dimnames in the binding dimension
bbind = function(..., .along = 1) {
  args = list(...)
  along = .along
  if(is.null(names(args))) {
    no_names = T
  } else if(any(names(args) == "")) {
    stop("all arguments must be named")
  } else {
    arg_names = names(args)
    no_names = F
  }

  # if any input is not a 'bbound' object then promote it
  n_args = length(args)
  args = lapply(args, bprep)
  args_dims = lapply(args, dim_or_length)

  # check that all arguments have compatible dimensions
  ref_dims = args_dims[[1]]
  n_dims = length(ref_dims)

  stopifnot(along > 0)
  if(along > n_dims) {
    stop("bbind: along dimension ",along," > number of dimensions ", n_dims)
  }
  for (i in seq_along(args)) {
    arg = args[[i]]
    ds = args_dims[[i]]
    if(length(ds) != n_dims)
      stop("bbind: dimension count mismatch in argument ",i)
    if(!all(ds[-along] == ref_dims[-along]))
      stop("bbind: non-binding dimensions mismatch in argument ",i)
  }

  # allocate space for the result
  bind_lengths = vapply(args_dims, function(x) x[along], 0)
  out_dims = ref_dims
  out_dims[along] = sum(bind_lengths)

  first = args[[1]]
  ref_dbs = dimblocks(first)
  ref_dns = dimnames_or_names(first)
  out = array(0, dim = out_dims)
  storage.mode(out) = storage.mode(first)

  # fill and track block info. initialize output dimblocks to the
  # dimblocks of the first element for all dimensions except along.
  out_blocks = vector("list", n_dims)
  for(d in seq_len(n_dims)[-along]) {
    out_blocks[d] = list(ref_dbs[[d]])
  }
  # now fill in the blocks for the binding dimension,
  # at the same time as we fill in the output array
  if(no_names) {
    out_blocks[along] = list(NULL);
  } else {
    out_blocks[[along]] = vector("list", 0)
  }

  # same, for output dimnames
  out_dimnames = vector("list", n_dims)
  for(d in seq_len(n_dims)[-along]) {
    # FHE 25 May 2025 use [ in case RHS is NULL
    out_dimnames[d] = ref_dns[d]
  }
  if(no_names) {
    out_dimnames[along] = list(NULL)
  } else {
    out_dimnames[[along]] = vector("character", out_dims[along])
  }
  start = 1
  for (i in seq_along(args)) {
    a = args[[i]]
    len = args_dims[[i]][[along]]
    end = start + len - 1

    # get indices for block assignment
    idx = vector("list", n_dims)
    for (j in seq_len(n_dims)[-along]) {
      idx[[j]] = seq_len(dim(a)[j])
    }
    idx[[along]] = start %upto% end
    # assign the block to the corresponding output array indices
    out = do.call(`[<-`, c(list(out), idx, list(value = a)))

    if(!no_names) {
      block_name = arg_names[[i]]

      # fill out_blocks
      ablocks = dimblocks(a)[[along]]
      out_blocks[[along]][[block_name]] =
        list(ixs = start %upto% end,
          names = dimnames_or_names(a)[[along]],
          subblocks = ablocks)
      # fill out_dimnames
      anas = dimnames_or_names(a)[[along]]
      if(!is.null(anas)) {
        out_dimnames[[along]][start:end] =
          paste0(block_name, "$", anas)
      } else {
        out_dimnames[[along]][start:end] =
          paste0(block_name, "$", as.character(seq_len(len)))
      }
    } else {
      # if we're operating in no-names mode, then we expect no
      # dimnames or dimblocks in the along dimension for any of the
      # arguments
      if(!is.null(dimblocks(a)[[along]])) {
        stop("bbind: no_names mode but non-null dimblocks in argument ",i)
      }
      if(!is.null(dimnames(a)[[along]])) {
        stop("bbind: no_names mode but non-null dimnames in argument ",i)
      }
    }

    start = end+1
  }

  dimnames(out) = out_dimnames
  dimblocks(out) = out_blocks
  out
}

# bfetch: fetch a block path from the block tree
# return the indices
bfetch = function(obj, dim, path) {
  stopifnot(!is.null(dimblocks(obj)))
  stopifnot("bbound" %in% class(obj))

  parts = strsplit(path, "$", fixed = TRUE)[[1]]
  blocks = dimblocks(obj)[[dim]]

  ixs = seq_len(dim_or_length(obj)[dim])
  for(p in parts) {
    if(is.null(blocks[[p]]))
      stop("Block not found: ", p)
    blk = blocks[[p]]
    ixs = ixs[blk$ixs]
    blocks = blk$subblocks
  }
  ixs
}


# subscript method for bbound
`[.bbound` = function(x, ..., drop=FALSE) {
  subs = missing_to_NA(...)
  dn = dim(x)
  nd = length(dn)
  idx = vector("list", nd)

  for (d in seq_len(nd)) {
    arg = subs[[d]]
    if (is.character(arg)) {
      stopifnot(length(arg) == 1)
      ixs = bfetch(x, arg, dim = d)
      idx[[d]] = ixs
    } else if(is.numeric(arg)) {
      # if numeric, just assume it is meant to be a numeric index
      idx[[d]] = arg
    } else if(length(arg)==1 && is.na(arg)) {
      # missing
      idx[[d]] = seq_len(dim(x)[d])
    } else {
      stop("Unknown argument type for [: ",sv(arg))
    }
  }

  # FHE 21 May 2025
  # tried using NextMethod here but couldn't get it to work
  args = c(list(un_bbound(x)),idx,drop=drop)
  do.call("[", args)
}

# TODO
# - test various 'drop' options
# - test interaction with tape_wrap etc.
