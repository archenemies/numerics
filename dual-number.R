# FHE 24 Mar 2025
# class for dual numbers in R
# from https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0

dual_number = function(value, dual=value*0) {
  if (!is.numeric(value) || !is.numeric(dual)) {
    stop("Both primal and dual must be numeric")
  }
  if (!identical(dim(value), dim(dual))) {
    stop("Primal and dual must have the same dimensions")
  }
  structure(list(value = value, dual = dual), class = "dual_number")
}

is.dual = function(x) { inherits(x, "dual_number") }

print.dual_number = function(x) {
  cat("dual_number:\n")
  cat("  primal\n")
  print(x$value)
  cat("  dual\n")
  print(x$dual)
}

collapse_dual = function(x, delta) {
  x$value + delta*x$dual
}

# we can use this to check operations numerically:
## > collapse_dual(x,0.01)/collapse_dual(y,0.01)
## [1] 1.512563
## > collapse_dual(x/y,0.01)
## [1] 1.5125

create_dual_method = function(op, dual_op) {
  # Get the original function for the operation
  op_func = get(op, envir = .GlobalEnv)
  # Define the S3 method name (e.g., "+.dual_number")
  method_name = paste0(op, ".dual_number")
  
  # Define the wrapper function
  wrapper_func = function(...) {
    args = list(...)
    args = lapply(args, eval.parent)
    # create unwrapped arguments for op_func, leaving non-numerics
    # unchanged
    unwrapped_args = lapply(args, function(arg) {
      if (is.dual(arg)) { arg$value } else { arg }
    })
    # create wrapped arguments for dual_op
    wrapped_args = lapply(args, function(arg) {
      if (is.numeric(arg)) { dual_number(arg, 0 * arg) } else { arg }
    })
    
    primal_res = do.call(op_func, unwrapped_args)
    dual_res = do.call(dual_op, wrapped_args)
    dual_number(primal_res, dual_res)
  }
  
  # Register the method in the global environment
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

# ----------------------------------------------------------------
# dual operations

# Addition: (a + b*dx) + (c + d*dx) = (a + c) + (b + d)*dx
dual_plus = function(e1, e2) {
  e1$dual + e2$dual
}
create_dual_method("+", dual_plus)

# Subtraction (binary and unary):
# Binary: (a + b*dx) - (c + d*dx) = (a - c) + (b - d)*dx
# Unary: -(a + b*dx) = (-a) + (-b)*dx
dual_minus = function(e1, e2 = NULL) {
  if (is.null(e2)) {
    -e1$dual  # Unary minus
  } else {
    e1$dual - e2$dual  # Binary subtraction
  }
}
create_dual_method("-", dual_minus)

# Multiplication: (a + b*dx) * (c + d*dx) = (a * c) + (b * c + a * d)*dx
dual_mult = function(e1, e2) {
  e1$dual * e2$value + e1$value * e2$dual
}
create_dual_method("*", dual_mult)

# Division: (a + b*dx) / (c + d*dx) = (a / c) + ((b * c - a * d) / c^2)*dx
dual_div = function(e1, e2) {
  (e1$dual * e2$value - e1$value * e2$dual) / (e2$value)^2
}
create_dual_method("/", dual_div)

# transpose
dual_t = function(x) {
  return(t(x$dual))
}
create_dual_method("t", dual_t)

# matrix multiplication
dual_mat_mult = function(x,y) {
  (x$value %*% y$dual) +
    (x$dual %*% y$value)
}
create_dual_method("%*%", dual_mat_mult)

# ----------------------------------------------------------------

mysource("check-dual-ops.R")

if(1) { # testing
  x = dual_number(3, 1)  # 3 + 1*dx (derivative = 1)
  y = dual_number(2, -1)  # 2 + -1*dx
  z = x * y
  # Primal: 3 * 2 = 6
  # Dual: (1 * 2 + 3 * 0) = 2
  pv(z)

  m = matrix(1:4, nrow = 2)
  r = t(dual_number(m, m*2))
  pv(r)

  check_dual_op("+")(x,y)
  check_dual_op("/")(x,y)
  check_dual_op("-")(x,y)
  check_dual_op("*")(x,y)
  check_dual_op("t")(r)
  check_dual_op("%*%")(r,r)
}
