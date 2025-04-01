# FHE 26 Mar 2025
# backpropagation using tape-wrap.R

back_plus = function(adj_out, val, e1, e2) {
  list(adj_out, adj_out)  # ∂(e1+e2)/∂e1 = 1, ∂(e1+e2)/∂e2 = 1
}

back_minus = function(adj_out, val, e1, e2) {
  if (missing(e2)) {  # unary minus
    list(-adj_out)    # ∂(-e1)/∂e1 = -1
  } else {            # binary
    list(adj_out, -adj_out)  # ∂(e1-e2)/∂e1 = 1, ∂(e1-e2)/∂e2 = -1
  }
}

back_mult = function(adj_out, val, e1, e2) {
  list(adj_out * e2, adj_out * e1)  # ∂(e1*e2)/∂e1 = e2, ∂(e1*e2)/∂e2 = e1
}

basic_back_ops = list(
  "+" = back_plus,
  "-" = back_minus,
  "*" = back_mult
)

# Helper function to get descendants of x and initialize accumulators
get_desc = function(x) {
  stop_if_no_tape()
  stopifnot(is.tape_wrap(x))
  tape <- .tape
  n <- tape$length
  if (n == 0 || x$id > n) stop("Tape is empty or does not contain x")

  # Initialize result list and visited set
  desc <- list()
  visited <- integer()
  to_process <- x$id  # Start with x

  # Forward traversal to find descendants
  while (length(to_process) > 0) {
    current_id <- to_process[1]
    to_process <- to_process[-1]

    if (current_id %in% visited) next
    visited <- c(visited, current_id)

    # Get the current entry
    entry <- tape$get(current_id)
    desc[[as.character(current_id)]] <- numeric(length(entry$value)) * 0  # Zero accumulator

    # Find entries that use current_id as an input
    for (i in 1:n) {
      if (i %in% visited) next
      next_entry <- tape$get(i)
      if (current_id %in% next_entry$inputs) {
        to_process <- c(to_process, i)
      }
    }
  }

  desc
}

# Gradient calculation with backprop
grad = function(x, y) {
  stop_if_no_tape()
  stopifnot(is.tape_wrap(x), is.tape_wrap(y), length(y$value) == 1)  # y must be scalar
  tape <- .tape
  n <- tape$length
  if (y$id > n) stop("Tape does not contain y")

  # Step 1: Get descendants of x and initialize accumulators
  accumulators <- get_desc(x)
  if (!(as.character(x$id) %in% names(accumulators))) {
    stop("x is not an ancestor of y")
  }

  # Step 2: Backpropagate from y
  accumulators[[as.character(y$id)]] <- 1  # Seed with ∂y/∂y = 1

  for (i in y$id:1) {
    # Skip if not a descendant of x
    if (!(as.character(i) %in% names(accumulators))) next

    entry <- tape$get(i)
    if (entry$op == "tape_var") next  # No inputs to propagate to

    # Get backward function and input values
    back_func <- basic_back_ops[[entry$op]]
    if (is.null(back_func)) stop("No backward rule for ", entry$op)
    inputs <- lapply(entry$inputs, function(id) tape$get(id)$value)

    # Compute gradients for inputs
    adj_in <- do.call(back_func, c(list(adj_out = accumulators[[as.character(i)]], val = entry$value), inputs))

    # Accumulate adjoints for inputs that are descendants
    for (j in seq_along(entry$inputs)) {
      input_id <- entry$inputs[j]
      if (as.character(input_id) %in% names(accumulators)) {
        accumulators[[as.character(input_id)]] <- accumulators[[as.character(input_id)]] + adj_in[[j]]
      }
    }
  }

  # Step 3: Return gradient for x
  accumulators[[as.character(x$id)]]
}

# Test example
if (TRUE) {
  tape_init()
  x <- tape_var(2)    # x = 2
  y <- tape_var(3)    # y = 3
  z <- x * y + x      # z = x*y + x = 2*3 + 2 = 8

  # Compute gradient ∂z/∂x
  g <- grad(x, z)
  cat("Gradient ∂z/∂x =", g, "\n")

  # Show tape and descendants for debugging
  show_tape()
  desc <- get_desc(x)
  cat("Descendants of x (IDs and zero accumulators):\n")
  for (id in names(desc)) {
    cat("ID", id, ":", desc[[id]], "\n")
  }
}
