# FHE 24 Mar 2025
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0
# edited by adding eval.parent call to get it to work
# was test-num-wrapper-2.R

# define the num_wrapper class
num_wrapper <- function(value) {
  structure(list(value = value), class = "num_wrapper")
}

# list of operators/functions to override
basic_ops <- c("+", "*", "-", "/", "t", "%*%", "solve")

# Function to create and assign the method
create_method <- function(op) {
  # the original function
  op_func <- get(op)
  
  # the method name
  method_name <- paste0(op, ".num_wrapper")
  
  # define the wrapper function. we'll fill in the argument names
  # later
  wrapper_func <- function() {
    # get the arguments passed to the function as a list:
    args <- as.list(match.call())[-1] # -1: exclude the function name itself
    args <- lapply(args, eval.parent)
    unwrapped_args <- lapply(args, function(arg) {
      # match.call gives us language objects for some reason,
      # must use eval:
      if (inherits(arg, "num_wrapper")) {
        arg$value
      } else {
        arg
      }
    })
    # now call op_func
    result <- do.call(op_func, unwrapped_args)
    # and return the wrapped result
    num_wrapper(result)
  }
  
  # fill in the arguments of wrapper_func to match op_func
  formals(wrapper_func) <- formals(args(op_func))
  
  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

for (op in basic_ops) {
  create_method(op)
}
