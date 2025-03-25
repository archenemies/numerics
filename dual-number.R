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
    # prepend primal_res to the argument list in case the function
    # needs it (like dual_solve)
    dual_args = c(list(primal_res), wrapped_args)
    dual_res = do.call(dual_op, dual_args)
    dual_number(primal_res, dual_res)
  }
  
  # Register the method in the global environment
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

# ----------------------------------------------------------------
# dual operations

# Addition: (a + b*dx) + (c + d*dx) = (a + c) + (b + d)*dx
dual_plus = function(., e1, e2) {
  e1$dual + e2$dual
}

# Subtraction (binary and unary):
# Binary: (a + b*dx) - (c + d*dx) = (a - c) + (b - d)*dx
# Unary: -(a + b*dx) = (-a) + (-b)*dx
dual_minus = function(., e1, e2 = NULL) {
  if (is.null(e2)) {
    -e1$dual  # Unary minus
  } else {
    e1$dual - e2$dual  # Binary subtraction
  }
}

# Multiplication: (a + b*dx) * (c + d*dx) = (a * c) + (b * c + a * d)*dx
dual_mult = function(., e1, e2) {
  e1$dual * e2$value + e1$value * e2$dual
}

# Division: (a + b*dx) / (c + d*dx) = (a / c) + ((b * c - a * d) / c^2)*dx
dual_div = function(val, e1, e2) {
  # val is e1$value / e2$value, we can use that here
  (e1$dual - val * e2$dual) / e2$value
  ## (e1$dual * e2$value - e1$value * e2$dual) / (e2$value)^2
}

# transpose
dual_t = function(., x) {
  return(t(x$dual))
}

# matrix multiplication
# XXX for mdual, we need a more general matrix multiplication
# which it seems must be done by rearranging dimensions?
# in the first term they are in the right order already
# but for the second we need a t() to get the final dimension
# first in front of last one and then another to get it after it again
# -- which asks the question, should we be dualizing a more general
# matrix multiplication function in the first place? or our users will
# just be doing the same dimension shuffling so this is good
dual_mat_mult = function(val, x,y) {
  message("HERE");
  (x$value %*% y$dual) + (x$dual %*% y$value)
}

dual_solve = function(val, a, b, ...) {
  stopifnot(nrow(a) == ncol(a))
  if(!missing(b)) {
    # simplify division into inverse + mult
    (solve(a) %*% b)$dual
  } else {
    # (don't know where this comes from)
    # d/du A^-1 = - A^-1 d/du (A) A^-1
    # don't forget the minus!
    - val %*% (a$dual %*% val)
  }
}

# realizing it's bad to automatically
# promote the arguments to functions to dual_number
# because we can't easily distinguish them from the 'dims'
# argument. better to just ask user to call dual_number
dual_rowSums = function(., x, dims) {
  stop("not implemented")
}

# need: rowSums, colSums
basic_dual_ops = list(
  "+"=dual_plus,
  "-"=dual_minus,
  "*"=dual_mult,
  "/"=dual_div,
  "t"=dual_t,
  "%*%"=dual_mat_mult,
  "solve"=dual_solve
)

basic_ops = names(basic_dual_ops)
# 'for' causes broken lexical scoping so use lapply
lapply(seq_along(basic_ops), function(i) {
#  pv(i,basic_ops[[i]],basic_dual_ops[[i]])
  create_dual_method(
    basic_ops[[i]],
    basic_dual_ops[[i]])
})

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
  check_dual_op("solve")(r)
}
