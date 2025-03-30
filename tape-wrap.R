# FHE 25 Mar 2025
# from num-wrap.R

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
show_tape = function() {
  # XXX print in columns and use maximum field width
  with(.tape, {
    cat("Tape of length ",length, "\n");
    for(i in 1 %upto% length) {
      ent = buf[[i]]
      cat("id=",ent$id, " op=",deparse(ent$op),
        " inputs=",deparse(ent$inputs),
        " repr=",deparse(ent$repr),
        " value=",deparse(ent$value),
        "\n")
    }
  })
}

tape_init <- function() {
  .tape <<- new_tape()
}
stop_if_no_tape <- function() {
  if(!exists(".tape")) {
    stop("Need to call tape_init first")
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
      message("Assigning ",name," = ",deparse(value))
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
  cat("tape_wrap:\n")
  cat("  op=",x$op,", repr=",x$repr,"\n")
  cat("  inputs=",deparse(x$inputs),"\n")
  cat("  value\n")
  print(x$value)
}

# list of operators/functions to override
basic_ops <- c("+", "*", "-", "/", "t", "%*%", "solve")

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
        tape_var_repr(arg,repr)
      } else { arg }
    })
    # the primary quantity:
    result <- do.call(op_func, unwrapped_args)
    # the wrapped quantity:
    input_ids = sapply(wrapped_args, function(tv) { tv$id })
    input_reprs = lapply(wrapped_args, function(tv) { tv$repr })
    new_repr = paste0(op, "(", paste0(input_reprs, collapse=","), ")")
    tape_wrap(result, op, input_ids, repr=new_repr)
  }

  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

for (op in basic_ops) {
  create_method(op)
}

mysource("export.R")

test_tape1 = function() {
  # compute (1+2)*2*5
  y <- tape_var(2);
  pv(y)
  tape_var(x=1, v=5);
  pv(x,v)
  z <- x+y
  pv(z)
  w <- z*tape_var(2)
  qq <- w*v
  pv(qq)
  export(x,y,z,w,v,qq)
}

if(1) {
  tape_init()
  test_tape1()
  show_tape()
}
