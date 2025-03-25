# Function to check dual number operation using numerical differentiation
check_dual_op <- function(op, delta = 1e-7, tol = 1e-5) {
  # Get the original operation function
  op_func <- get(op, envir = .GlobalEnv)

  # from wrapper_func in create_dual_method
  checker_func = function() {
    args = as.list(match.call())[-1]
    args = lapply(args, eval.parent)
    unwrapped_args = lapply(args, function(arg) {
      if (is.dual(arg)) { arg$value } else { arg }
    })
    unwrapped_delta_args = lapply(args, function(arg) {
      if (is.dual(arg)) { collapse_dual(arg,delta) } else { arg }
    })
    wrapped_args = lapply(args, function(arg) {
      if (is.numeric(arg)) { dual_number(arg) } else { arg }
    })
    
    u_res = do.call(op_func, unwrapped_args)
    u_delta_res = do.call(op_func, unwrapped_delta_args)
    dual_res = do.call(op_func, wrapped_args)

    num_deriv = (u_delta_res-u_res)/delta
    an_deriv = dual_res$dual
    dev = max(abs(num_deriv-an_deriv))
    good = dev < tol
    if(!good) {
      message("Failed ",op,": ", dev, ">=", tol)
      pv(u_res, u_delta_res, dual_res)
      pv((u_delta_res-u_res)/delta)
      pv(dual_res$dual)
    } else {
      message("Passed ",op,": ", dev, "<", tol)
    }
    invisible(good)
  }
  formals(checker_func) = formals(args(op_func))
  checker_func
}
