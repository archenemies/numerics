# FHE 25 Mar 2025
# from num-wrap.R

mysource("expanding_list.R")

new_tape <- function() {
  buf <- list()
  length <- 0
  next_id <- function() { length+1 }
  add <- function(val) {
    length <<- length + 1
    buf[[length]] <<- val
  }
  get <- function(ix) {
    buf[[ix]]
  }
  environment()
}
show_tape = function() {
  with(.tape, {
    cat("Tape of length ",length, "\n");
    for(i in 1 %upto% length) {
      ent = buf[[i]]
      cat("id=",ent$id, " op=",ent$op," inputs=",deparse(ent$inputs),": ",ent$repr,"\n")
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

# define the tape_wrap class
tape_wrap <- function(value, op, inputs, repr=deparse(substitute(value))) {
  stop_if_no_tape()
  stopifnot(is.integer(inputs))
  stopifnot(is.character(repr))
  res = structure(
    list_vars(value, repr, op, inputs),
    class = "tape_wrap")
  res$id = .tape$next_id()
  .tape$add(res)
  res
}

tape_var <- function(value) {
  repr = deparse(substitute(value))
  tape_wrap(value, repr, "tape_var", integer())
}

is.tape_wrap <- function(x) { inherits(x, "tape_wrap") }

# list of operators/functions to override
basic_ops <- c("+", "*", "-", "/", "t", "%*%", "solve")

# Function to create and assign the method
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
      # YYY in future must change is.numeric to include e.g. duals
      if(!is.tape_wrap(arg) && is.numeric(arg)) {
        tape_var(arg, repr=repr)
      } else { arg }
    })
    # XXX need wrapped_args for inputs
    # XXX need string for valstr
    # XXX need tape_var() (op="tape_var", no inputs)
    # now call op_func
    result <- do.call(op_func, unwrapped_args)
    input_ids = lapply(wrapped_args, function(tv) { tv$id })
    # and return the wrapped result
    # XXX
    tape_wrap(result, op, input_ids)
  }
  
  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

for (op in basic_ops) {
  create_method(op)
}

if(1) {
  tape_init()
  x=tape_var(1); y=tape_var(2);
  z = x+y
  ## x=1; y=2;
  ## z = tape_var(x) + tape_var(y)
  pv(z)
}
