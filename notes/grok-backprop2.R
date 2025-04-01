# a lot of these seem to be wrong?
# grad traverses the tape too many times?

# FHE 26 Mar 2025
# backpropagation using tape-wrap.R

# Backward rules for operations
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

back_div = function(adj_out, val, e1, e2) {
  list(adj_out / e2, -adj_out * e1 / (e2 * e2))  # ∂(e1/e2)/∂e1 = 1/e2, ∂(e1/e2)/∂e2 = -e1/e2^2
}

back_subset = function(adj_out, val, e1, i) {
  # ∂(e1[i])/∂e1 = 1 at index i, 0 elsewhere
  grad <- array(0, dim(e1))
  grad[i] <- adj_out
  list(grad)
}

back_transpose = function(adj_out, val, e1) {
  list(t(adj_out))  # ∂(t(e1))/∂e1 = t(adj_out)
}

back_aperm = function(adj_out, val, e1, perm) {
  # Inverse permutation to propagate gradient
  iperm <- order(perm)
  list(aperm(adj_out, iperm))  # ∂(aperm(e1,perm))/∂e1 = aperm(adj_out, iperm)
}

back_matmult = function(adj_out, val, e1, e2) {
  list(adj_out %*% t(e2), t(e1) %*% adj_out)  # ∂(e1 %*% e2)/∂e1 = adj_out * e2^T, ∂/∂e2 = e1^T * adj_out
}

back_solve = function(adj_out, val, e1, e2) {
  # ∂(solve(e1,e2))/∂e1 = -val %*% adj_out %*% t(val), ∂/∂e2 = val^T
  list(-val %*% adj_out %*% t(val), t(val))
}

back_sum = function(adj_out, val, e1) {
  list(rep(adj_out, length(e1)))  # ∂(sum(e1))/∂e1 = 1 for each element
}

back_rowSums = function(adj_out, val, e1) {
  list(matrix(adj_out, nrow=nrow(e1), ncol=ncol(e1), byrow=FALSE))  # ∂(rowSums(e1))/∂e1 = 1 per row
}

back_colSums = function(adj_out, val, e1) {
  list(matrix(adj_out, nrow=nrow(e1), ncol=ncol(e1), byrow=TRUE))  # ∂(colSums(e1))/∂e1 = 1 per column
}

basic_back_ops = list(
  "+" = back_plus,
  "-" = back_minus,
  "*" = back_mult,
  "/" = back_div,
  "[" = back_subset,
  "t" = back_transpose,
  "aperm" = back_aperm,
  "%*%" = back_matmult,
  "solve" = back_solve,
  "sum" = back_sum,
  "rowSums" = back_rowSums,
  "colSums" = back_colSums
)

# Gradient calculation with backprop
grad = function(x, y) {
  stop_if_no_tape()
  stopifnot(is.tape_wrap(x), is.tape_wrap(y), length(y$value) == 1)  # x numeric, y scalar

  tape <- .tape
  n <- tape$length
  if (n == 0 || y$id > n || x$id > n) stop("Tape does not contain x or y")

  # Step 1: Find descendants of x up to y
  descendants <- integer(0)
  to_visit <- x$id
  visited <- logical(n)
  while (length(to_visit) > 0) {
    current_id <- to_visit[1]
    to_visit <- to_visit[-1]
    if (visited[current_id]) next
    visited[current_id] <- TRUE
    descendants <- c(descendants, current_id)
    entry <- tape$get(current_id)
    # Find entries that use current_id as an input
    for (i in (current_id + 1):min(y$id, n)) {
      next_entry <- tape$get(i)
      if (any(next_entry$inputs == current_id) && !visited[i]) {
        to_visit <- c(to_visit, i)
      }
    }
  }
  if (!y$id %in% descendants) stop("y is not a descendant of x")

  # Step 2: Initialize adjoints for descendants only
  adjoints <- numeric(n)  # 0 for non-descendants
  adjoints[y$id] <- 1     # Seed with ∂y/∂y = 1

  # Step 3: Backpropagate from y, only for descendants
  for (i in rev(descendants)) {
    entry <- tape$get(i)
    if (entry$op == "tape_var") next  # No inputs to propagate

    back_func <- basic_back_ops[[entry$op]]
    if (is.null(back_func)) stop("No backward rule for ", entry$op)

    # Get input values (or pass extra args like perm for aperm)
    inputs <- lapply(entry$inputs, function(id) tape$get(id)$value)
    if (entry$op == "[") inputs <- c(inputs, list(entry$repr))  # Pass index
    if (entry$op == "aperm") inputs <- c(inputs, list(attr(entry, "perm")))  # Pass perm

    # Compute gradients for inputs
    adj_in <- do.call(back_func, c(list(adj_out = adjoints[i], val = entry$value), inputs))

    # Accumulate adjoints for inputs that are descendants
    for (j in seq_along(entry$inputs)) {
      input_id <- entry$inputs[j]
      if (input_id %in% descendants) {
        adjoints[input_id] <- adjoints[input_id] + adj_in[[j]]
      }
    }
  }

  # Return gradient for x
  adjoints[x$id]
}

# Numerical gradient test
test_grad = function(xs, y, eps = 1e-6) {
  if (is.tape_wrap(xs)) xs <- list(xs)
  stopifnot(all(sapply(xs, is.tape_wrap)), is.tape_wrap(y))

  # Analytical gradients
  grad_analytical <- sapply(xs, function(x) grad(x, y))

  # Numerical gradients
  grad_numerical <- numeric(length(xs))
  base_y <- y$value

  for (i in seq_along(xs)) {
    tape_init()  # Reset tape
    xs_perturbed <- lapply(xs, function(x) tape_var(x$value))  # Recreate vars
    xs_perturbed[[i]]$value <- xs_perturbed[[i]]$value + eps   # Perturb
    # Recompute y (assuming y$op and inputs align with original computation)
    y_perturbed <- eval(parse(text = y$repr), envir = list2env(xs_perturbed))

    grad_numerical[i] <- (y_perturbed - base_y) / eps
  }

  # Compare
  cat("Analytical gradients:", grad_analytical, "\n")
  cat("Numerical gradients: ", grad_numerical, "\n")
  diff <- abs(grad_analytical - grad_numerical)
  cat("Differences: ", diff, "\n")
  all(diff < eps * 10)  # Tolerance
}

# Test example
if (TRUE) {
  tape_init()
  x <- tape_var(2)          # x = 2
  y <- tape_var(3)          # y = 3
  z <- x * y / (x + y)     # z = (2*3)/(2+3) = 6/5 = 1.2

  # Compute gradients
  grad_x <- grad(x, z)
  grad_y <- grad(y, z)
  cat("Gradients: ∂z/∂x =", grad_x, ", ∂z/∂y =", grad_y, "\n")
  show_tape()

  # Verify numerically
  cat("\nTesting gradients:\n")
  test_result <- test_grad(list(x, y), z)
  cat("Gradients match (within tolerance):", test_result, "\n")
}
