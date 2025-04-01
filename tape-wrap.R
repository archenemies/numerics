# FHE 25 Mar 2025
# from num-wrap.R

.tape <- NULL

# define the tape_wrap class
tape_wrap <- function(value, op, inputs, repr=deparse(substitute(value))) {
  stop_if_no_tape()
  # FHE 30 Mar 2025 sanity check, remove this if you actually want to
  # double-wrap:
  stopifnot(!is.tape_wrap(value))
  stopifnot(is.integer(inputs))
  stopifnot(is.character(repr))
  res = structure(
    list_vars(value, repr, op, inputs),
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
  cat("Tape of length ",tp$length, "\n");
  # just create a data frame with the tape data, and print it
  ents = tp$buf
  df = as.data.frame(do.call(rbind,ents))
  # put ID column first
  ids = as.numeric(df$id)
  df$id = NULL
  df = cbind(id=ids,df)
  # row.names are just ids, so suppress them
  print(df,row.names=F)
  # TODO: use maximum field width to truncate long strings
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

# list of operators/functions to override
basic_ops <- c("+", "*", "-", "/", "t", "%*%", "solve")

crop_repr <- function(str) {
  # https://stackoverflow.com/questions/46759358/truncate-character-strings-after-first-n-characters
  ifelse(nchar(str) > 13, paste0(strtrim(str, 10), '...'), str)
  }

# Function to create and assign a tape_wrap method for 'op'
create_method <- function(op) {
  # the original function
  op_func <- get(op)

  # the method name
  method_name <- paste0(op, ".tape_wrap")

  # define the wrapper function. we'll fill in the argument names
  # later
  wrapper_func <- function(...) {
    args <- list(...);
    unwrapped_args <- lapply(args, function(arg) {
      if(is.tape_wrap(arg)) { arg$value } else { arg }
    })
    arg_text = lapply(substitute(...()),deparse)
    wrapped_args <- lapply(seq_along(args), function(i) {
      arg = args[[i]]
      repr = arg_text[[i]]
      # wrap all arguments, even strings and integers
      if(!is.tape_wrap(arg)) {
        stop("All arguments should be wrapped: op=",op)
#        tape_var_repr(arg,repr)
      } else { arg }
    })
    # the primary quantity:
    result <- do.call(op_func, unwrapped_args)
    # the wrapped quantity:
    input_ids = sapply(wrapped_args, function(tv) { tv$id })
    input_reprs = lapply(wrapped_args, function(tv) { tv$repr })
    cropped = lapply(input_reprs, crop_repr)
    new_repr = paste0(op, "(", paste0(cropped, collapse=","), ")")
    tape_wrap(result, op, input_ids, repr=new_repr)
  }

  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

for (op in basic_ops) {
  create_method(op)
}

# special operations

# we don't expect dim() to return a wrapped value
dim.tape_wrap = function(x) {
  dim(x$value)
}

if(mySourceLevel==0) {
  mysource("test-tape-wrap.R")
  test_tape2()
}
