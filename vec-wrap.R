# FHE 03 Apr 2025
# vec-wrap.R
# class vec_wrap for wrapping vector operations

# The original purpose of this class is to facilitate vectorized tape
# computations in backprop.R. If we wrap an array in this class, the
# wrapped object appears to be missing the last dimension, and its
# methods automatically perform vectorized operations over that
# dimension.

# usage:
# vec_wrap(A, recycle=F)
# vec_wrap_of(A)

# - A is an array
#  - the first usage creates a new object whose apparent dimensions are those of A, minus the last dimension. it stores A as the value
#  - the second usage combines vec_wrap(recycle=T) and vector_of, creating a vec_wrap representing a constant value
#  - the object stores the recycle flag and the value
#  - if recycle=T then the last dimension of value should be 1

vec_wrap <- function(value, recycle=F) {
  message("in vec_wrap: ",deparse(value))
  stopifnot(dim(value) != NULL)
  n = tail(dim(value));
  if(recycle) stopifnot(n==1);
  structure(list_vars(value, recycle, n), class = "vec_wrap")
}

vec_wrap_of = function(value) {
  message("in vec_wrap_of: ",deparse(value))
  vec_wrap(vector_of(value),recycle=T)
}

is.vec_wrap = function(x) { inherits(x, "vec_wrap") }

print.vec_wrap = function(x) {
  cat("vec_wrap: (recycle=",as.character(x$recycle),")\n")
  print(x$value)
}

# this will only do anything if we also source
# mysource("generic-deparse.R")
deparse.vec_wrap = function(x) {
  if(x$recycle==T) {
    paste0("vec_wrap(",deparse(x$value),",recycle=T)")
  } else {
    paste0("vec_wrap(",deparse(x$value),")")
  }
}

# use create_method for operations that are automatically vectorized
# correctly the result is something like this but handles recycling
# more correctly:
## +.vec_wrap = function(e1,e2) {
##   vec_wrap(e1$value + e2$value)
## }
## *.vec_wrap = function(e1,e2) {
##   vec_wrap(e1$value + e2$value)
## }

unvecwrap = function(x) {stopifnot(is.vec_wrap(x)); x$value}

# from num-wrap.R
# Function to create and assign the wrapped method for each operation
create_vec_wrap_method <- function(op) {
  # the original function
  op_func <- get(op)

  # the method name
  method_name <- paste0(op, ".vec_wrap")

  # define the wrapper function. we'll fill in the argument names
  # later
  wrapper_func <- function(...) {
    # get the arguments passed to the function as a list:

    # match.call gives us language objects for some reason, and we
    # couldn't figure out how to use these without creating a double
    # evaluation. See history.
    args <- list(...);

    if(!all(sapply(args, is.vec_wrap))) {
      stop("All arguments must be wrapped")
    }
    rs = sapply(args,function(x){x$recycle})
    arg_dims = lapply(args,function(x){dim(x$value)})
    if(all(rs)) {
      # if all inputs are recycling, so are we
      out_r = T
      out_dim = arg_dims[[1]]
    } else {
      out_dim = arg_dims[!rs][[1]]
      out_r = F
    }
    out_n = tail(out_dim,1)
    out_wdim = head(out_dim,-1)
    for(i in seq_along(args)) {
      if(rs[i]) { # recycled
        iwdim = head(arg_dims[[i]],-1)
        if(!identical(iwdim, out_wdim)) {
          stop("vec_wrap: Wrong dimensions for recycled object at arg index ",i,
            " op=",op," wdim=",deparse(iwdim))
        }
      } else {
        idim = arg_dims[[i]]
        if(!identical(idim, out_dim)) {
          stop("vec_wrap: Wrong dimensions for object at arg index ",i,
            " op=",op," dim=",deparse(idim))
        }
      }
    }
    # we need to convert everything to vectors so that recycling
    # happens correctly. recall that the last dimension is the slowest
    # to change in R
    unwrapped_args <- lapply(args,
      function(x) { as.vector(unvecwrap(x)) }
    )
    # now call op_func
    res <- do.call(op_func, unwrapped_args)

    # convert from vector back to array
    dres = array(res, dim=out_dim)

    # and return the wrapped result
    vec_wrap(dres, recycle=out_r)
  }

  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

auto_vec_ops <- c("+", "*", "-", "/",
  "exp","log",
  "sin","cos","tan") # etc.

for (op in auto_vec_ops) {
  create_vec_wrap_method(op)
}

dim.vec_wrap = function(x) {
  head(dim(x$value),-1)
}

# XXX
# - aperm, t, %*%, vec_mat_mult
# - i hope the "recycling" optimization is worth it but there may be more to do
#   - for example move the logic from create_vec_wrap_method into helper functions so that we can update the examples +.vec_wrap and have them still look good
#   - this will be needed before we start on %*% etc.
