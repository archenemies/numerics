# FHE 25 Mar 2025
# from num-wrap.R

mysource("ops-common.R")

.tape <- NULL

# define the tape_wrap class
tape_wrap <- function(value, op, inputs, repr=deparse(substitute(value)),
  extra_args=NULL) {
  stop_if_no_tape()
  # FHE 30 Mar 2025 sanity check, remove this if you actually want to
  # double-wrap:
  stopifnot(!is.tape_wrap(value))
  stopifnot(is.integer(inputs))
  stopifnot(is.character(repr))
  res = structure(
    list_vars(value, repr, op, inputs, extra_args),
    class = "tape_wrap")
  res$id = .tape$next_id()
  .tape$add(res)
  res
}

new_tape <- function() {
  buf <- list()
  length <- 0L
  next_id <- function() { length+1L }
  add <- function(val) {
    length <<- length + 1L
    buf[[length]] <<- val
  }
  get <- function(ix) {
    stopifnot(ix <= length)
    stopifnot(ix >= 1)
    buf[[ix]]
  }
  environment()
}
current_tape = function() {
  return(.tape)
}
free_tape = function(tp=.tape) {
  # format(.tape) -> "<environment: 0x62ef0d0ea768>"
  if(format(tp)==format(.tape)) {
    warning("Freeing the current tape")
    .tape <<- NULL
  }
  with(tp, {rm(list=ls())})
  invisible(NULL)
}

show_tape = function(tp=.tape) {
  stopifnot(!is.null(tp))
  cat("Tape of length ",tp$length, "\n");
  # just create a data frame with the tape data, and print it
  ents = tp$buf
  df = as.data.frame(do.call(rbind,ents))
  # put ID column first
  ids = as.numeric(df$id)
  df$id = NULL
  df = cbind(id=ids,df)

  # print the dimensions
  df$dim = lapply(df$value, dim)

  # crop the (sometimes long) value strings with ...
  df$value = sapply(df$value, Curry(crop_str,n=15) %c% Curry(paste0,collapse=", ") %c% format)

  df$extra_args = sapply(df$extra_args, deparse1)
  ## df$extra_args = sapply(df$extra_args, Curry(crop_str,n=6) %c% deparse1)

  # row.names are just ids, so suppress them
  print(df,row.names=F)
}

use_tape <- function(tp) {
  .tape <<- tp
}
stop_if_no_tape <- function() {
  if(!exists(".tape")) {
    stop("Shouldn't get here")
  }
  if(is.null(.tape)) {
    stop("Need to call use_tape first")
  }
}

# wrap a value in a tape_wrap object with no inputs
# creates an "independent variable"
tape_var <- function(...) {
  args = list(...)
  reprs = substitute(...()) # gets deparsed later
  for(i in seq_along(args)) {
    value = args[[i]]
    name = names(args)[[i]]
    repr = deparse(reprs[[i]])
    if(!is.null(name) && name != "") {
      repr = paste0(name, "=", repr)
    }
    res = tape_wrap(value, "tape_var", integer(), repr=repr)
    if(!is.null(name) && name != "") {
      # message("Assigning ",name," = ",deparse(value))
      assign(name, res, envir=parent.frame())
    }
  }
  # return the value of the last assignment
  res
}
tape_var_repr <- function(value, repr) {
  tape_wrap(value, "tape_var", integer(), repr=repr)
}

is.tape_wrap <- function(x) { inherits(x, "tape_wrap") }

print.tape_wrap <- function(x) {
  cat("tape_wrap: op=",x$op,", repr=",x$repr,"\n")
  cat("  inputs=",deparse(x$inputs),"\n")
  cat("  value:\n")
  print(x$value)
}

# this will only do anything if we also source
# mysource("generic-deparse.R")
deparse.tape_wrap = function(tw) {
  paste0("tape_wrap(",deparse(tw$value),
    ",",deparse(tw$op),
    ",",deparse(tw$inputs),
    ",repr=",deparse(tw$repr),")")
}

untapewrap = function(tw) {stopifnot(is.tape_wrap(tw)); tw$value}

################################################################
# operations

# list of operators/functions to override
basic_ops <- c("+", "*", "-", "/", "t", "%*%", "solve",
  "exp", "log"
  )

# operations with extra (non-wrapped) arguments are defined separately
# below. these go in extra_args in the tape_wrap object.

sum.tape_wrap = function(x, na.rm=F) {
  tape_method_dispatch(sum, "sum", list(x), list(na.rm=na.rm))
}

rep.tape_wrap = function(x, ...) {
  tape_method_dispatch(rep, "rep", list(x), list(...))
}
array.tape_wrap = function(data, dim) {
  tape_method_dispatch(array, "array", list(data), list(dim=dim))
}

length.tape_wrap = function(l) {
  length(l$value)
}
as.vector.tape_wrap = function(x, mode) {
  tape_method_dispatch(as.vector, "as.vector", list(x), list(mode=mode))
}
`[.tape_wrap` = function(x, ...) {
  tape_method_dispatch(`[`, "[", list(x), list(...))
}
# subscript assignment
`[<-.tape_wrap` = function(x, ..., value) {
  # we need to create a new tape_var to store the result
  val = x$value
  val[...] <- value$value
  input_ids = c(x$id,value$id)
  opname = "subscr_assign" # see ops-common.R
  cropped = paste0(crop_repr(x$repr),", ",crop_repr(value$repr))
  new_repr = paste0(opname, "(", cropped, ")")
  tape_wrap(val, opname, input_ids, repr=new_repr, extra_args=list(...))
}

# emit cell with no inputs
zeros_like.tape_wrap = function(x, ...) {
  tape_var(zeros_like(x$value, ...))
}

# we don't expect dim() to return a wrapped value so it is not
# considered a tape operation
dim.tape_wrap = function(x) {
  dim(x$value)
}
is.numeric.tape_wrap = function(x) { TRUE }

# helper for create_method
crop_repr = Curry(crop_str, n=10)

# https://stackoverflow.com/questions/46759358/truncate-character-strings-after-first-n-characters
crop_str <- function(str, n=13) {
  ifelse(nchar(str) > n, paste0(strtrim(str, n-3), '...'), str)
}

tape_method_dispatch = function(fn, opname, args, extra_args=NULL) {
#  message("tape_method_dispatch: ", sv(opname, args))
  # FHE 09 Apr 2025 break out of create_method
  if(!all(sapply(args, is.tape_wrap))) {
    stop("All arguments must be wrapped")
  }
  unwrapped_args <- lapply(args, untapewrap)

  # the primary quantity:
  result <- do.call(fn, c(unwrapped_args, extra_args))
  # the wrapped quantity:
  input_ids = sapply(args, function(tv) { tv$id })
  input_reprs = lapply(args, function(tv) { tv$repr })
  cropped = lapply(input_reprs, crop_repr)
  new_repr = paste0(opname, "(", paste0(cropped, collapse=","), ")")
  tape_wrap(result, opname, input_ids, repr=new_repr, extra_args=extra_args)
}

# Function to create and assign a tape_wrap method for the function
# 'op'. This is called for all simple operators, for which there are
# no extra (non-numeric) arguments.
create_tape_method <- function(op) {
  # the original function
  op_fun <- get(op)

  # define the wrapper function. we'll fill in the argument names
  # later
  wrapper_func <- function(...) {
    args <- list(...);
    tape_method_dispatch(op_fun, op, args, NULL)
  }

  # the method name
  method_name <- paste0(op, ".tape_wrap")
  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

for (op in basic_ops) {
  create_tape_method(op)
}

if(mySourceLevel==0) {
  mysource("test-tape-wrap.R")
  test_tape2()
}
