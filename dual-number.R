# FHE 24 Mar 2025
# class for dual numbers in R
# for sources see num-wrap.R, tape-wrap.R
# also https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa?show_subscribe=0

mysource("tape-wrap.R")

dual_number = function(value, dual=zeros_like(value)) {
## dual_number = function(value, dual=value*0) {
  if(!is.numeric(value) || !is.numeric(dual)) {
    stop("Both primal and dual must be numeric")
  }
  if(!identical(dim(value), dim(dual))) {
    stop("Primal and dual must have the same dimensions")
  }
  if(is.tape_wrap(value) != is.tape_wrap(dual)) {
    stop("dual_number safety check: only one argument is tape_wrap")
  }
  structure(list(value = value, dual = dual), class = "dual_number")
}

dual_var = function(value) {
  dual_number(value, ones_like(value))
}

is.dual_number = function(x) { inherits(x, "dual_number") }

print.dual_number = function(x) {
  cat("dual_number:\n")
  cat("  primal\n")
  print(x$value)
  cat("  dual\n")
  print(x$dual)
}

deparse.dual_number = function(dn,...) {
  paste0("dual_number(",deparse1(dn$value),
    ",",deparse1(dn$dual),")")
}

collapse_dual = function(x, delta) {
  x$value + delta*x$dual
}

# we can use this to check operations numerically:
## > collapse_dual(x,0.01)/collapse_dual(y,0.01)
## [1] 1.512563
## > collapse_dual(x/y,0.01)
## [1] 1.5125

undualnumber = function(x) { stopifnot(is.dual_number(x)); x$value }

create_dual_method = function(op, dual_op) {
  # Get the original function for the operation
  op_func = get(op, envir = .GlobalEnv)
  # Define the S3 method name (e.g., "+.dual_number")
  method_name = paste0(op, ".dual_number")

  # Define the wrapper function
  wrapper_func = function(...) {
    args = list(...)

    # create unwrapped arguments for op_func, leaving non-numerics
    # unchanged
    unwrapped_args = lapply(args, function(arg) {
      ## if (is.dual_number(arg)) { arg$value } else { arg }
      stopifnot(is.dual_number(arg))
      arg$value
    })

    primal_res = do.call(op_func, unwrapped_args)
    # prepend primal_res to the argument list in case the function
    # needs it (like dual_solve)
    dual_args = c(list(primal_res), args)
    dual_res = do.call(dual_op, dual_args)
    dual_number(primal_res, dual_res)
  }

  # Register the method in the global environment
  assign(method_name, wrapper_func, envir = .GlobalEnv)
}

# ----------------------------------------------------------------
# non-op methods

zeros_like.dual_number = function(x, ...) {
  dual_number(zeros_like(x$value, ...),
    zeros_like(x$value))
}
length.dual_number = function(x) {
  length(x$value)
}
dim.dual_number = function(x) {
  dim(x$value)
}
is.numeric.dual_number = function(x) { TRUE }

# ----------------------------------------------------------------
# dual operations
mysource("ops-common.R")

# Addition: (a + b*dx) + (c + d*dx) = (a + c) + (b + d)*dx
dual_plus = function(., e1, e2) {
  e1$dual + e2$dual
}

# Subtraction (binary and unary):
# Binary: (a + b*dx) - (c + d*dx) = (a - c) + (b - d)*dx
# Unary: -(a + b*dx) = (-a) + (-b)*dx
dual_minus = function(., e1, e2) {
  if (missing(e2)) {
    -e1$dual  # Unary minus
  } else {
    e1$dual - e2$dual  # Binary subtraction
  }
}

# Multiplication: (a + b*dx) * (c + d*dx) = (a * c) + (b * c + a * d)*dx
dual_mult = function(., e1, e2) {
  stopifnot(is.dual_number(e1))
  stopifnot(is.dual_number(e2))
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

# we need a more general matrix multiplication for mdual. note that
# for a vector dual, in the first term they are in the right order
# already but for the second we need a t() to get the final dimension.
# if we just use vec_wrap, it will end up doing a useless array
# transpose on the replicated x$value...
  # vector duals:
# (i,j) %*% (j,k,l) + (i,j,l) %*% (j,k)
dual_mat_mult = function(val, x, y) {
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

dual_exp = function(val, x) {
  val * x$dual
}
dual_log = function(val, x) {
  x$dual / x$value
}

dual_subscr = function(val, x, ...) {
  x$dual[...]
}

`[.dual_number` = function(x, ...) {
  val = x$value[...]
  vd = dual_subscr(val, x, ...)
  dual_number(val,vd)
}

# see ops-common.R
# this is called by backprop for "subscr_assign" dual op
# it has a better argument order
dual_subscr_assign = function(x, value, ...) {
  x$value[...] = value$value
  x$dual[...] = value$dual
  invisible(x)
}

`[<-.dual_number` = function(x, ..., value) {
  dual_subscr_assign(x, value, ...)
}

# helpers for use in dual-only tape traversal
dual_rowSums = function(val, x, na.rm=F, dims=1) {
  rowSums(x$dual, na.rm=na.rm, dims=dims)
}
rowSums.dual_number = function(x, na.rm=F, dims=1) {
  val = rowSums(x$value, na.rm=na.rm, dims=dims)
  vd = dual_rowSums(val, x, na.rm, dims)
  dual_number(val,vd)
}

dual_colSums = function(val, x, na.rm=F, dims=1) {
  colSums(x$dual, na.rm=na.rm, dims=dims)
}
colSums.dual_number = function(x, na.rm=F, dims=1) {
  val = colSums(x$value, na.rm=na.rm, dims=dims)
  vd = dual_colSums(val, x, na.rm=na.rm, dims=dims)
  dual_number(val,vd)
}

dual_sum = function(val, x, na.rm=F, dims=1) {
  sum(x$dual, na.rm=na.rm)
}
sum.dual_number = function(x, na.rm=F) {
  val = sum(x$value, na.rm=na.rm)
  vd = dual_sum(val, x, na.rm=na.rm)
  dual_number(val,vd)
}

dual_cumsum = function(val, x) {
  cumsum(x$dual)
}

# use ... so we can have n= or each=
dual_rep = function(val, x, ...) {
  rep(x$dual, ...)
}

# replicate a dual_number
rep.dual_number = function(x, ...) {
  val = rep(x$value, ...)
  vd = dual_rep(val, x, ...)
  dual_number(val, vd)
}

# TODO automate generation of these three methods (with dual_plus)
dual_c = function(val, ...) {
  args = list(...)
  do.call(c, lapply(args, function(x) {x$dual}))
}
dual_cbind = function(val, ...) {
  args = list(...)
  do.call(cbind, lapply(args, function(x) {x$dual}))
}
dual_rbind = function(val, ...) {
  args = list(...)
  do.call(rbind, lapply(args, function(x) {x$dual}))
}

dual_array = function(val, data, dim) {
  array(data$dual, dim)
}

# shape a dual_number vector
array.dual_number = function(data, dim) {
  val = array(data$value, dim)
  vd = dual_array(val, data, dim)
  dual_number(val, vd)
}

# list of all the dual operations we have defined
# see operations.txt for a full list
basic_dual_ops = list(
  "+"=dual_plus,
  "-"=dual_minus,
  "*"=dual_mult,
  "/"=dual_div,
  "t"=dual_t,
  "%*%"=dual_mat_mult,
  "solve"=dual_solve,
  "exp"=dual_exp,
  "log"=dual_log,
  "c"=dual_c,
  "rbind"=dual_rbind,
  "cbind"=dual_cbind,
  "cumsum"=dual_cumsum
)
# operations defined specially: sum, rowSums, colSums, etc.
# because one or more arguments is not a dual_number
# these are needed here for tape traversal (??)
dual_ops = c(basic_dual_ops,
  list("sum"=dual_sum,
    "["=dual_subscr,
    "rowSums"=dual_rowSums,
    "colSums"=dual_colSums,
    "array"=dual_array,
    "rep"=dual_rep
  )
)

# generate methods for the basic operations
# 'for' causes broken lexical scoping so use lapply
lapply(names(basic_dual_ops), function(nm) {
  create_dual_method(nm, basic_dual_ops[[nm]])
})

if(mySourceLevel==0) {
  mysource("test-dual-number.R")
  test_dual_ops()
}
