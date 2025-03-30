# FHE 26 Mar 2025
# backpropagation using tape-wrap.R

# Backward rules for operations
basic_back_ops = list(
  "+" = back_plus,
  "-" = back_minus,
  "*" = back_mult
)

back_plus = function(adj_out, val, e1, e2) {
  list(adj_out, adj_out)  # ∂(e1+e2)/∂e1 = 1, ∂(e1+e2)/∂e2 = 1
}

back_minus = function(adj_out, val, e1, e2) {
  if (missing(e2)) {  # Unary minus
    list(-adj_out)    # ∂(-e1)/∂e1 = -1
  } else {            # Binary minus
    list(adj_out, -adj_out)  # ∂(e1-e2)/∂e1 = 1, ∂(e1-e2)/∂e2 = -1
  }
}

back_mult = function(adj_out, val, e1, e2) {
  list(adj_out * e2, adj_out * e1)  # ∂(e1*e2)/∂e1 = e2, ∂(e1*e2)/∂e2 = e1
}

# Gradient calculation with backprop
grad = function(xs, y) {
  stop_if_no_tape()
  # Ensure xs is a list
  if (is.tape_wrap(xs)) xs <- list(xs)
  stopifnot(all(sapply(xs, is.tape_wrap)))
  stopifnot(is.tape_wrap(y), length(y$value) == 1)  # y must be scalar
  
  # Step 1: Collect all tape entries (assume all are descendants for simplicity)
  tape <- .tape
  n <- tape$length
  if (n == 0 || y$id > n) stop("Tape does not contain y or is empty")
  
  # Step 2: Initialize adjoint accumulators
  adjoints <- numeric(n)  # Vector indexed by ID
  adjoints[y$id] <- 1     # Seed with ∂y/∂y = 1
  
  # Step 3: Backpropagate from y to earlier entries
  for (i in y$id:1) {
    entry <- tape$get(i)
    if (entry$op == "tape_var") next  # No inputs to propagate to
    
    # Get the backward function and input values
    back_func <- basic_back_ops[[entry$op]]
    if (is.null(back_func)) stop("No backward rule for ", entry$op)
    inputs <- lapply(entry$inputs, function(id) tape$get(id)$value)
    
    # Compute gradients for inputs
    adj_in <- do.call(back_func, c(list(adj_out = adjoints[i], val = entry$value), inputs))
    
    # Accumulate adjoints for each input
    for (j in seq_along(entry$inputs)) {
      input_id <- entry$inputs[j]
      adjoints[input_id] <- adjoints[input_id] + adj_in[[j]]
    }
  }
  
  # Extract gradients for xs
  sapply(xs, function(x) adjoints[x$id])
}

# Numerical gradient test
test_grad = function(xs, y, eps = 1e-6) {
  if (is.tape_wrap(xs)) xs <- list(xs)
  stopifnot(all(sapply(xs, is.tape_wrap)), is.tape_wrap(y))
  
  # Analytical gradients
  grad_analytical <- grad(xs, y)
  
  # Numerical gradients
  grad_numerical <- numeric(length(xs))
  base_y <- y$value
  
  for (i in seq_along(xs)) {
    # Perturb the i-th input
    tape_init()  # Reset tape for fresh computation
    xs_perturbed <- lapply(xs, function(x) tape_var(x$value))  # Recreate vars
    xs_perturbed[[i]]$value <- xs_perturbed[[i]]$value + eps   # Perturb
    y_perturbed <- do.call(get(y$op), lapply(xs_perturbed, function(x) x$value))
    
    # Numerical derivative
    grad_numerical[i] <- (y_perturbed - base_y) / eps
  }
  
  # Compare
  cat("Analytical gradients:", grad_analytical, "\n")
  cat("Numerical gradients: ", grad_numerical, "\n")
  diff <- abs(grad_analytical - grad_numerical)
  cat("Differences: ", diff, "\n")
  all(diff < eps * 10)  # Allow some tolerance
}

# Test example
if (TRUE) {
  tape_init()
  x <- tape_var(2)    # x = 2
  y <- tape_var(3)    # y = 3
  z <- x * y + x      # z = x*y + x = 2*3 + 2 = 8
  
  # Compute gradients
  grads <- grad(list(x, y), z)
  cat("Gradients: ∂z/∂x =", grads[1], ", ∂z/∂y =", grads[2], "\n")
  show_tape()
  
  # Verify with numerical differentiation
  cat("\nTesting gradients numerically:\n")
  test_result <- test_grad(list(x, y), z)
  cat("Gradients match (within tolerance):", test_result, "\n")
}
