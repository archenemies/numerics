# FHE 24 Mar 2025
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0

# Define the num_wrapper class
num_wrapper <- function(value) {
  structure(list(value = value), class = "num_wrapper")
}

# List of operators to override
bin_ops <- c("+", "*", "-", "/")

# Dynamically define methods for each operator
for (op in bin_ops) {
  (function(op) {
    # Create the method name, e.g., "+.num_wrapper"
    method_name <- paste0(op, ".num_wrapper")
    # Assign a function to that method name in the global environment
    assign(method_name, function(e1, e2) {
      # Extract value from e1 if it’s a num_wrapper, otherwise use e1 as is
      if (inherits(e1, "num_wrapper")) {
        val1 <- e1$value
      } else {
        val1 <- e1
      }

      # Extract value from e2 if it’s a num_wrapper, otherwise use e2 as is
      if (inherits(e2, "num_wrapper")) {
        val2 <- e2$value
      } else {
        val2 <- e2
      }

      # Get the operator function (e.g., `+`, `*`) and apply it
      op_func <- get(op)
#      browser()
      num_wrapper(op_func(val1, val2))
    }, envir = .GlobalEnv)
  }) (op)
}
