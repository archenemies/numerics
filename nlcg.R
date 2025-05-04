# FHE 30 Apr 2025
# Implementation of NLCG optimization method
# making use of dual_number.R and backprop.R

# FHE 30 Apr 2025 transcribed from written notes

# x: current value of the main independent variable
# d: current direction
# obj_fn: returns list(y, grad)
# hvp_fn: same as obj_fn but accepts (x,xdot) and returns dual component
nlcg_step = function(x, d, last_grad_x, obj_fn, hvp_fn) {
  # we need 1 gradient and 1 HVP
  # can calculate these at the same time if we can pert and dual

# from rt/rt-source/gradient_descent_plus.py
## beta_denom = np.inner(last_grad_x, last_grad_x)
## assert beta_denom > 0, "Zero gradient in NLCG, do you have any training points?"
## beta = np.inner(grad_x, grad_x - last_grad_x) / beta_denom
## if beta<0: dmsg(f"nlcg restart, beta={beta}")
## beta = max(0,beta)
## nlcg_d = nlcg_d * beta - grad_x
## # A*d
## ad = mult_by_hessian(nlcg_d)
## # the minus sign comes from r = -grad_x
## alpha = - np.inner(nlcg_d, grad_x) / np.inner(nlcg_d, ad)
## x = x + alpha * nlcg_d

  list(x=xnew, d=dnew)
}

cg_step = function(x, d, hvp_fn) {
  # XXX
  # like nlcg_step, but don't update x
}

# main NLCG function
# note that we need to specialize this with additional methods to accept tape_var or dual_number inputs.
# we need to create a new tape to evaluate the objective
opt_nlcg = function(x0, d0=NULL, objective, max_steps=100, tol=1e-3, ...) {
  local_tape()
# XXX above executes these in parent frame
  push_tape(new_tape())
  on.exit({pop_tape()})

  extra_args = list(...)
  tape_var(.x=x0)
  exs = lapply(tape_var, extra_args)
  .y = do.call(objective, c(list(.x), exs))
  .grad = tape_get_grad(.y,.x,wrap=T)

  # calculate the objective function and gradient from the tape
  obj_fn = function(xnew) {
    tape_get_perts(list(.x), list(xnew),
      list(.y, .grad))
  }
  # accepts a dual number and gives dual_numbers for .y and .grad
  hvp_fn = function(xdual) {
    tape_get_perts(list(.x), xdual,
      list(.y, .grad), promote=dual_number)
  }
  # as above but accepts dual numbers for each of the extra independent variables.
  # we create this function here for later use in back_nlcg and dual_nlcg
  extra_dual_fn = function(...) {
    new_exs = list(...)
    tape_get_perts(exs, new_exs,
      list(.y, .grad), promote=dual_number)
  }

  step=0
  last_change=Inf
  x=x0
  d=.grad$value
  track_logger(step, x, d, last_change)
  while(step<max_steps && last_change>tol) {
    res = nlcg_step(x,d,obj_fn,hvp_fn)
    x=res$x
    d=res$d
    track_logger(step, x, d, last_change)
  }

  attr(x,"functions") = list_vars(obj_fn, hvp_fn, extra_dual_fn)
  x
}
