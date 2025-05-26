# FHE 30 Apr 2025
# Implementation of NLCG optimization method
# making use of dual_number.R and backprop.R

# FHE 30 Apr 2025 transcribed from written notes

# XXX eventually improve NLCG algorithm to deal with local maxima?

# initialize a state object for NLCG algorithm
# x: current value of the main independent variable
# obj_fn: returns list(y, grad)
# hvp_fn: same as obj_fn but accepts (x,xdot) and returns dual component
nlcg_new_state = function(x, obj_fn, hvp_fn) {
  step = 0
  last_grad_x = NULL
  last_change = Inf
  nlcg_d = NULL
  x0 = x
  as.environment(list_vars(
    step,
    x,
    x0,
    last_grad_x,
    last_change,
    nlcg_d,
    obj_fn, hvp_fn
  ))
}

# state: from nlcg_new_state(x, obj_fn, hvp_fn)
#nlcg_step = function(x, d, last_grad_x, obj_fn, hvp_fn) {
nlcg_step = function(state) {
  # we need 1 gradient and 1 HVP
  # can calculate these at the same time if we can pert and dual

  nlcg_d = state$nlcg_d
  last_grad_x = state$last_grad_x
  step = state$step
  
  obj_res = obj_fn(x)
  obj_val = obj_res[[1]]
  grad_x = obj_res[[2]]
  if(is.null(last_grad_x)) {
    message("nlcg_step: first run, setting last_grad_x to grad_x")
    stopifnot(step==0)
    last_grad_x = grad_x
  }
  if(is.null(nlcg_d)) {
    message("nlcg_step: first run, setting nlcg_d to -grad_x")
    stopifnot(step==0)
    nlcg_d = -grad_x
  }
  step = step + 1

# from rt/rt-source/gradient_descent_plus.py
## beta_denom = np.inner(last_grad_x, last_grad_x)
  beta_denom = sum(last_grad_x**2)
## assert beta_denom > 0, "Zero gradient in NLCG, do you have any training points?"
  if(beta_denom == 0)
    stop("Zero gradient in NLCG, do you have any training points?")
## beta = np.inner(grad_x, grad_x - last_grad_x) / beta_denom
  beta = sum(grad_x * (grad_x-state$last_grad_x)) / beta_denom
## if beta<0: dmsg(f"nlcg restart, beta={beta}")
  if(beta<0) message("nlcg restart, beta=",beta);
## beta = max(0,beta)
  beta = max(0,beta)
## nlcg_d = nlcg_d * beta - grad_x
  nlcg_d = nlcg_d * beta - grad_x
## # A*d
## ad = mult_by_hessian(nlcg_d)
  ad = hvp_fn(dual_number(x,nlcg_d))[[1]]
## # the minus sign comes from r = -grad_x
## alpha = - np.inner(nlcg_d, grad_x) / np.inner(nlcg_d, ad)
  alpha = - sum(nlcg_d * grad_x) / sum(nlcg_d * ad)
## x = x + alpha * nlcg_d
  xdelta = alpha * nlcg_d
  xnew = x + xdelta

  last_change = sum(xdelta**2)

  state$nlcg_d = nlcg_d
  state$last_grad_x = grad_x
  state$last_change = last_change
  state$step = step
  state$x = xnew

  message("nlcg_step: ",sv(step, steplen, obj_val))
  
  # XXX track logger stuff here, check if x wants to be logged

  xnew
}

# CG answers the question, what dual component should we give to x so
# that the dual of the gradient has a certain value? it is the same as
# multiplying the supplied gradient dual by the inverted Hessian.
# like nlcg_step, but don't update x, just xdot.
# also equivalent to finding the step required to restore the gradient to zero,
# assuming quadratic shape, if we have added sum(x*grad_dual) to the objective.
cg_step = function(cg_state) {
  # XXX need cg_new_state (just hvp_fn, no objective, and supply grad_dot)
  # XXX implement CG algorithm
}

# main NLCG function
# note that we need to specialize this with additional methods to accept tape_var or dual_number inputs.
# we need to create a new tape to evaluate the objective
opt_nlcg = function(x0, d0=NULL, objective, max_steps=100, tol=1e-3, ...) {
  local_tape()

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
      list(.grad), promote=dual_number)
  }
  # as above but accepts dual numbers for each of the extra independent variables.
  # we create this function here for later use in back_nlcg and dual_nlcg
  extra_dual_fn = function(...) {
    new_exs = list(...)
    tape_get_pert(exs, new_exs,
      list(.y, .grad), promote=dual_number)
  }

  # initialize NLCG and run to convergence
  state = nlcg_new_state(x, obj_fn, hvp_fn)
  while(state$step<max_steps && state$last_change>tol) {
    nlcg_step(state)
  }
  x = state$x

  # store derived functions for back_ops and dual_ops
  attr(x,"opt_nlcg_functions") = list_vars(obj_fn, hvp_fn, extra_dual_fn)
  x
}
