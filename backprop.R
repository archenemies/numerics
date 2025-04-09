# -*- my-source-delegate: "test-backprop.R" -*-
# FHE 26 Mar 2025
# backpropagation using tape-wrap.R
mysource("tape-wrap.R")

# ----------------------------------------------------------------
# Operations:
# Every operation must have a back_ version, as in back_add, back_mat_mul, ... This takes as input the adjoint of the operator output, and returns a list of adjoints, one for each input (with NA for non-tape inputs)
#     back_add = function(adj_out, val, e1, e2) {
#         list(adj_out, adj_out) }
#     back_mul = function("""") {
#         list(adj_out*e2, adj_out*e1) }

# this is needed for tape_var tape cells
# it returns NULL because the single input is not a tape_var
back_tape_var = function(adj_out, val, value) {
  list(NULL)
}
back_plus = function(adj_out, val, e1, e2) {
  stopifnot(length(e1)==length(e2))
  list(adj_out, adj_out)
}
back_minus = function(adj_out, val, e1, e2) {
  if(missing(e2)) { # unary minus
    list(-adj_out)
  } else { # binary
    stopifnot(length(e1)==length(e2))
    list(adj_out, -adj_out)
  }
}
# e1 and e2 are (in simple case) unwrapped
back_mult = function(adj_out, val, e1, e2) {
  stopifnot(length(e1)==length(e2))
  list(adj_out*e2, adj_out*e1)
}
# simplify using 'val'
back_div = function(adj_out, val, e1, e2) {
  stopifnot(length(e1)==length(e2))
  tm = adj_out/e2
  list(tm,
    -tm*val)
}

back_exp = function(adj_out, val, x) {
  list(adj_out*val)
}

back_log = function(adj_out, val, x) {
  list(adj_out/x)
}

back_sum = function(adj_out, val, x) {
  stopifnot(length(adj_out)==1)
  stopifnot(length(val)==1)

  # ones, with the same dims (and type) as x.
  # more optimal way to write x*0+1?
  #  list(x*0+1) <- we can't do it this way without rep()
  list(dim_like(rep(adj_out, length(x)), x))
}

# XXX note we don't handle 'each' yet, which may be necessary
back_rep = function(adj_out, val, x, n) {
  # adj_out has the larger length
  # it (and val) is n times as long as x

  # we need to use rowSums (and remember to set the output dimension
  # since it will be NULL if rowSums returns a vector) on adj_out to
  # produce a value with the same dimensions as arg

  L = length(adj_out)
  l = length(x)
  stopifnot(l*n == L)
  #
  m = array(adj_out, dim=c(n,l))
  # we sum the n repetitions over the l columns
  v = colSums(m)
  stopifnot(length(v)==l)
  # NULL is for the 'n' reps argument
  list(dim_like(v,x), NULL)
}

back_as.vector = function(adj_out, val, x) {
  # adj_out is a vector
  # x is a vector or array
  list(dim_like(adj_out, x))
}

back_array = function(adj_out, val, data, dims) {
  # adj_out has dimensions 'dims'
  # we just need to reshape it to have same dimensions as data

  # remember we return a gradient for each argument
  list(dim_like(adj_out, data), NULL)
}

basic_back_ops = list(
  "tape_var"=back_tape_var,
  "+"=back_plus,
  "-"=back_minus,
  "*"=back_mult,
  "/"=back_div,

  "exp"=back_exp,
  "log"=back_log,

  "sum"=back_sum,
  "rep"=back_rep,
  "as.vector"=back_as.vector,
  "array"=back_array
)
back_ops = basic_back_ops

#----------------------------------------------------------------

all_inputs = function(y) {
  inputs = integer(0)
  new_inputs = y$id
  while(length(new_inputs)>0) {
    next_iid = new_inputs[1]
    inputs = c(inputs, next_iid)
    new_inputs = new_inputs[-1]
    next_inputs = .tape$get(next_iid)$inputs
    new_inputs = sort(decreasing=TRUE,
      union(next_inputs, new_inputs))
  }
  inputs
}

# helper
list_exists = function(l,id) {
  if(id > length(l)) return(FALSE);
  return(!is.null(l[[id]]))
}

# thoughts. for the purpose of perturbing the inputs, or for
# propagating a dual number, we need a function that can traverse the
# tape and update the values depending on x into a separate list. we
# need to think about how to do this generally so that there is only
# one traversal function. we can have the same function help us with
# the accumulators. start with the accumulator version and keep it
# general.

# forward_traverse(x)
# x: the tape_var whose descendents we're interested in
# type: the type of traversal we want
#   - accum: return a list of zero'ed accumulators for grad
#   - pert: recompute parts of the computation
# xaux: auxiliary value for "pert" and "dual" types
# restrict_ids: restrict our attention to these tape cells (typically,
#   ancestors of an objective)
# wrap: if true then expect xaux to be a tape_wrap object, and produce
#   tape_wrap'ped output
forward_traverse = function(x, xaux=NULL, upto=NULL, type="init_accum",
  restrict_ids=NULL, wrap=FALSE) {
  stop_if_no_tape()
  n = .tape$length
  l = list()
  if(is.null(upto)) upto = .tape$get(n);
  if(is.null(restrict_ids)) restrict_ids = as.integer(x$id %upto% upto$id);
  has_id = function(id) { list_exists(l,id) }
  maybe_unwrap = if(wrap) { identity } else { untapewrap }
  maybe_wrap = if(wrap) { tape_var } else { identity }
  add_accum_entry = function(ent) {
    # the simplest traversal operation, just initialize all the
    # accumulators for dependents of x. in case wrap==T, we don't
    # actually depend on the original tape cell ent, so we create a
    # new tape_var with the zero value

    # zeros_like is generic so ent$value can be a wrapped numeric
    zeroval = zeros_like(ent$value)
    ## zeroval = ent$value*0
    # if recording, give accums a distinct repr:
    if(wrap) zeroval = tape_var_repr(zeroval,"init_accum")
    l[[ent$id]] <<- zeroval;
  }
  add_pert_entry = function(ent, dual=1) {
    # we add x specially. after that we call this
    # function with tape_wrap which we re-evaluate on perturbed inputs, and add to l
    inputs = ent$inputs
    pert_inputs = list()
    for(i in seq_along(inputs)) {
      iid = inputs[i]
      if(list_exists(l,iid)) {
        pert_inputs[[i]] = l[[iid]]
      } else {
        pert_inputs[[i]] = maybe_unwrap(.tape$get(iid))
      }
    }
    # if wrap is true then this will be the wrapped operation
    pert_output = do.call(ent$op, pert_inputs)
    l[[ent$id]] <<- pert_output
  }
  add_dual_entry = function(ent, dual=1) {
    # similar to add_pert_entry but with propagation of duals
    stop("not implemented")
  }
  add_entry = switch(type, init_accum=add_accum_entry,
    dual=add_dual_entry,
    pert=add_pert_entry,
    stop("Unknown get_accum_list type ", type))
  stopifnot(identical(x,.tape$get(x$id)))
  if(type=="init_accum") {
    add_entry(x)
  } else if(type=="dual" || type=="pert") {
    stopifnot(identical(dim(x$value), dim(xaux)))
    if(wrap) stopifnot(is.tape_wrap(xaux))
    l[[x$id]] = xaux;
  }
  # sort ascending
  restrict_ids = sort(restrict_ids)
  for(i in restrict_ids) {
    ent = .tape$get(i)
    inputs = ent$inputs
    if(any(sapply(inputs, has_id))) {
      add_entry(ent)
    }
  }
  return(l)
}

tape_get_pert = function(x,xaux,y) {
  stop_if_no_tape()
  # call forward_traverse(x, xaux=xaux, upto=y, type="pert")
  y_inputs = all_inputs(y)
  l = forward_traverse(x, xaux=xaux, type="pert", restrict_ids=y_inputs)
  l[[y$id]]
}

# ----------------------------------------------------------------
# tape_get_grad: gradient calculation with backprop
#   tape_get_grad(x,y) - x and y are tape_wrap's
#     x must be wrapping a numeric
#     y must be wrapping a scalar value
#   1. traverse tape and compile a list of descendents of x (up to y)
#      and initialize accumulators (one for each descendent)
#   2. work backwards from y, following inputs and accumulating adjoints in entries of the accumulator list from 1, until you get to x. if you find an input with no accumulator then we can ignore it since it is not a descendent of x.
#   3. return the x accumulator
# assume that .tape points to the current tape

# Notes: 1. this algorithm may not be optimal for all-paths (?) but its running time is dwarfed by the actual computation
# 2. we have opted not to add entries to the tape when computing the gradient, that would be a simple modification but we're not sure we would use it

# arguments: wrap, if true then record the gradient calculation on the tape
tape_get_grad = function(x,y,wrap=F) {
  stop_if_no_tape()
  stopifnot(length(y$value)==1)

  # call forward_traverse
  y_inputs = all_inputs(y)
  if(!x$id %in% y_inputs) {
    stop("tape_get_grad: y not a descendent of x")
  }

  accums = forward_traverse(x, type="init_accum",
    restrict_ids=y_inputs, wrap=wrap)
  stopifnot(list_exists(accums,y$id))

  y_adj = ones_like(y)
  if(wrap) y_adj = tape_var(y_adj)
  accums[[y$id]] = y_adj

  new_inputs = y$id
  while(length(new_inputs)>0) {
    next_iid = new_inputs[1]
    new_inputs = new_inputs[-1]
    # only recurse into descendents of x
    if(list_exists(accums, next_iid)) {
      ent = .tape$get(next_iid)
      inputs = ent$inputs
      adj_out = accums[[next_iid]]
      input_ents = .tape$buf[inputs]
      # what is tape_var doing with inputs? it creates a new tape_var for stuff like dim. so we can't assume that val is numeric
      # we need to gather the input values from the tape
      # then get the list of input adjoints from the back_OP in basic_back_ops
      if(!wrap) {
        val = ent$value
        unwrapped_inputs = lapply(input_ents, untapewrap)
        args = append(list(adj_out, val), unwrapped_inputs)
      } else {
        stopifnot(is.tape_wrap(adj_out))
        args = append(list(adj_out, ent), input_ents)
      }
      back_op = back_ops[[ent$op]]
      if(is.null(back_op)) {
        stop("Undefined back_op for: ",ent$op)
      }
      # get the list of input adjoints from adj_out and the other arguments
      res = do.call(back_op, args)
      # res is a list with NULL for non-numeric args to op
      # now accumulate the input adjoints in accum
      for(i in seq_along(res)) {
        if(!is.null(res[[i]])) {
          r = res[[i]]
          if(!wrap) { stopifnot(is.numeric(r)) }
          else { stopifnot(is.tape_wrap(r)) }
          stopifnot(length(inputs)>=i)
          iid = inputs[i]
          if(list_exists(accums, iid)) {
            if(!identical(dim(r),dim(accums[[iid]]))) {
              stop("Error: accumulator has wrong dimension: ",
                sv(dim(r))," != ",sv(dim(accums[[iid]])))
            }
            accums[[iid]] = accums[[iid]] + r
          }
        }
      }
      # make sure we eventually recurse on 'inputs'
      new_inputs = sort(decreasing=TRUE,
        union(inputs, new_inputs))
    }
  }
  accums[[x$id]]
}
# ----------------------------------------------------------------

# TODO:
# \- gradient calculation with backprop.
#
# \- a way to test it: a function test_grad(xs,y) that uses numerical differentiation to find the gradient and check it against the one returned by grad().
#   \- compare tape_get_pert with tape_get_grad
#   \-> check_tape_grad_pert

# - slightly more operations
# - test with vectors/matrices
# - functions for JVP and HVP
# - more operations

# Note: to do hvp (hessian-vector product):
#   - we must run the computation over again with a dual number input
#     - we can re-use the tape but only if there is special recognition for dual_number datatype
#     - grad_dual(x,y) where x is a dual number (or list of them)
#       - we need to use the tape to do another forward pass to propagate the duals from x to y
#       - then y will be a dual number
#       - y-primal doesn't depend on x-dual
#       - assume we already did a backward pass to get dy/dx (primal), now we need to do another backward pass to get d(dy/dt)/dx
#         - we just set the y-dual-adjoint to 1 and the y-adjoint to 0 for this second pass
#         - this calculates the hessian vector product (the y-dual being the jacobian vector product?) where the input vector is the x dual, dx/dt
#         - y = f(x(t)), dy/dt = f'(x(t))x'(t)
#           - d/dx(dy/dt) = f''(x) * x'
#           - is it written better in the paper?
#         - we are only interested in the backpropagated primal?
