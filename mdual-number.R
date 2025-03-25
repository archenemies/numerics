# FHE 24 Mar 2025
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0

mdual_number <- function(value, mdual, names) {
  # Validate inputs
  if (!is.numeric(value) || !is.array(mdual)) {
    stop("value must be numeric, mdual must be an array")
  }
  if (length(dim(mdual)) < 1) {
    stop("mdual must have at least one dimension")
  }
  if (dim(mdual)[1] != length(names)) {
    stop("First dimension of mdual must match length of names")
  }
  if (any(duplicated(names))) {
    stop("names must be unique")
  }
  
  # Get dimensions of value (handle scalar case)
  value_dim <- if (is.null(dim(value))) length(value) else dim(value)
  # Get dimensions of mdual excluding the first (variables)
  mdual_dim <- dim(mdual)[-1]
  # XXX want to exclude the last instead

  # XXX are these necessary?
  # Adjust for scalar value: mdual should be a vector
  if (length(dim(value)) == 0) {
    if (length(dim(mdual)) != 1) {
      stop("For scalar value, mdual must be a vector")
    }
    if (length(value) != 1) {
      stop("value must be a single number if scalar")
    }
  } else {
    # For non-scalar value, compare dimensions
    if (!identical(value_dim, mdual_dim)) {
      stop("Dimensions of mdual (excluding first) must match those of value")
    }
  }
  
  # Create and return the object
  structure(list(value = value, mdual = mdual, names = names), class = "mdual_number")
}

# XXX fill in the rest from dual-number.R
# XXX put me in a git repo
