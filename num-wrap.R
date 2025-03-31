# FHE 24 Mar 2025
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0
# edited by adding eval.parent call to get it to work
# was test-num-wrapper-2.R

# Not used except as an example

# define the num_wrap class
num_wrap <- function(value) {
  message("in num_wrap: ",deparse(value))
  structure(list(value = value), class = "num_wrap")
}

is.num_wrap = function(x) { inherits(x, "num_wrap") }

# list of operators/functions to override
basic_ops <- c("+", "*", "-", "/", "t", "%*%", "solve")

# Function to create and assign the method
create_method <- function(op) {
  # the original function
  op_func <- get(op)

  # the method name
  method_name <- paste0(op, ".num_wrap")

  # define the wrapper function. we'll fill in the argument names
  # later
  wrapper_func <- function(...) {
    # get the arguments passed to the function as a list:

    # match.call gives us language objects for some reason, and we
    # couldn't figure out how to use these without creating a double
    # evaluation. See history.
    args <- list(...);

    unwrapped_args <- lapply(args, function(arg) {
      # must use eval:
      if (is.num_wrap(arg)) {
        arg$value
      } else {
        arg
      }
    })
    # now call op_func
    result <- do.call(op_func, unwrapped_args)
    # and return the wrapped result
    num_wrap(result)
  }

  # fill in the arguments of wrapper_func to match op_func
  # (this only worked with match.call above:)
#  formals(wrapper_func) <- formals(args(op_func))

  # now install the wrapper function
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

for (op in basic_ops) {
  create_method(op)
}

if(0) {
  e1 = num_wrap(1); e2 = num_wrap(2); e1+e2
  num_wrap(1)+num_wrap(2)
}
