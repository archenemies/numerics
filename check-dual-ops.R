# FHE 24 Mar 2025

# from check_dual_op
# input is a list of arguments for fn
check_dual_function = function(fn, input, delta = 1e-7, tol = 1e-4,
  fname = deparse(substitute(fn))) {
  args = input
  unwrapped_args = lapply(args, undualnumber)

  unwrapped_delta_args = lapply(args, Curry(collapse_dual,delta=delta))

  u_res = do.call(fn, unwrapped_args)
  u_delta_res = do.call(fn, unwrapped_delta_args)
  dual_res = do.call(fn, args)

  num_deriv = (u_delta_res-u_res)/delta
  an_deriv = dual_res$dual
  dev = max(abs(num_deriv-an_deriv))
  good = dev < tol
  if(!good) {
    message("Failed ",fname,": ", dev, ">=", tol)
    pv(u_res, u_delta_res, dual_res)
    pv((u_delta_res-u_res)/delta)
    pv(dual_res$dual)
    stop("Failed")
  } else {
    message("Passed ",fname,": ", dev, "<", tol)
  }
  invisible(good)
}

# Function to check dual number operation using numerical differentiation
check_dual_op <- function(op, delta = 1e-7, tol = 1e-4) {
  # Get the original operation function
  op_func <- get(op, envir = .GlobalEnv)

  # from wrapper_func in create_dual_method
  function(...) {
    args = list(...)
    check_dual_function(op_func, fname=op, args, delta=delta, tol=tol)
  }
}
