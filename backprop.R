# -*- my-source-delegate: "test-backprop.R" -*-
# FHE 26 Mar 2025
# backpropagation using tape-wrap.R
mysource("tape-wrap.R")
mysource("dual-number.R")

# ----------------------------------------------------------------
# Operations:
# Every operation must have a back_ version, as in back_add,
# back_mat_mul, ... This takes as input the adjoint of the operator
# output, and returns a list of adjoints, one for each input (with NA
# for non-tape inputs)
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

back_sum = function(adj_out, val, x, ...) {
  stopifnot(length(adj_out)==1)
  stopifnot(length(val)==1)

  # ones, with the same dims (and type) as x.
  # more optimal way to write x*0+1?
  #  list(x*0+1) <- we can't do it this way without rep()
  list(dim_like(rep(adj_out, length(x)), x))
}

# the last argument can be each=reps or times=reps; with no name it
# defaults to times=reps
back_rep = function(adj_out, val, x, ...) {
  extra_args = list(...)
  stopifnot(length(extra_args)==1)
  each = F;
  if(names(extra_args)=="each") each=T
  reps=extra_args[[1]]

  # adj_out has the larger length
  # it (and val) is n times as long as x

  # we need to use rowSums (and remember to set the output dimension
  # since it will be NULL if rowSums returns a vector) on adj_out to
  # produce a value with the same dimensions as arg

  L = length(adj_out)
  l = length(x)
  stopifnot(l*reps == L)
  if(each) {
    # the last dimension changes most slowly
    m = array(adj_out, dim=c(reps,l))
    # we sum the n repetitions over the l columns of m
    v = colSums(m)
  } else {
    m = array(adj_out, dim=c(l,reps))
    # we sum the n repetitions over the l rows of m
    v = rowSums(m)
  }
  stopifnot(length(v)==l)
  list(dim_like(v,x))
}

back_c = function(adj_out, val, ...) {
  args = list(...)
  lens = lapply(args, length)
  offs = c(0,cumsum(lens))
  # loop through args
  lapply(seq_along(args),
    function(i) {
      dim_like(adj_out[(offs[i]+1) %upto% offs[i+1]],
        args[[i]])
    }
  )
}

back_cbind = function(adj_out, val, ...) {
  args = list(...)
  lens = lapply(args, ncol)
  # check that all inputs are matrices
  stopifnot(!any(as.logical(lapply(lens,is.null))))
  offs = c(0,cumsum(lens))
  nrs = lapply(args, nrow)
  for(i in 2 %upto% length(nrs)) {
    if(!identical(nrs[[i]],nrs[[1]])) {
      stop("Row count mismatch for cbind: ",
        sv(i,nrs[[1]],nrs[[i]]))
    }
  }

  # loop through args and subscript adj_out to get input adjoints
  lapply(seq_along(args),
    function(i) {
      dim_like(adj_out[,(offs[i]+1) %upto% offs[i+1]],
        args[[i]])
    }
  )
}

back_rbind = function(adj_out, val, ...) {
  # following back_cbind

  args = list(...)
  lens = lapply(args, nrow)
  stopifnot(!any(as.logical(lapply(lens,is.null))))
  offs = c(0,cumsum(lens))
  ncs = lapply(args, ncol)
  for(i in 2 %upto% length(ncs)) {
    if(!identical(ncs[[i]],ncs[[1]])) {
      stop("Column count mismatch for rbind: ",
        sv(i,ncs[[1]],ncs[[i]]))
    }
  }
  lapply(seq_along(args),
    function(i) {
      dim_like(adj_out[(offs[i]+1) %upto% offs[i+1],],
        args[[i]])
    }
  )
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

back_subscr = function(adj_out, val, x, ...) {
  x0 = zeros_like(x)
  if(length(adj_out) != length(x0[...])) {
    stop("Error in back_subscr")
  }
  x0[...] = adj_out
  list(x0) # remember to return a list!
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
  "c"=back_c,
  "cbind"=back_cbind,
  "rbind"=back_rbind,
  "as.vector"=back_as.vector,
  "array"=back_array,
  "["=back_subscr
)
back_ops = basic_back_ops

#----------------------------------------------------------------

find_all_inputs = function(ys) {
  inputs = integer(0)
  new_inputs = sapply(ys, function(y) {y$id});
  new_inputs = sort(decreasing=TRUE, new_inputs)
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

cell_rerun_zero = function(ent, l, wrap=F) {
  # the simplest traversal operation, just initialize all the
  # accumulators for dependents of x. in case wrap==T, we don't
  # actually depend on the original tape cell ent, so we create a
  # new tape_var with the zero value

  # zeros_like is generic so ent$value can be a wrapped numeric
  zeroval = zeros_like(ent$value)
  ## zeroval = ent$value*0
  # if recording, give accums a distinct repr:
  if(wrap) zeroval = tape_var_repr(zeroval,"init_accum")
  l[[ent$id]] <- zeroval
  l
}

# helper function from cell_rerun_pert for sharing with cell_rerun_dual
tape_gather_alt_inputs = function(ent, l, promote=identity) {
  inputs = ent$inputs
  alt_inputs = list()
  for(i in seq_along(inputs)) {
    iid = inputs[i]
    if(list_exists(l,iid)) {
      alt_inputs[[i]] = l[[iid]]
    } else {
      alt_inputs[[i]] = promote(.tape$get(iid))
    }
  }
  alt_inputs
}

cell_rerun_pert = function(ent, l, promote) {
  # we add x specially. after that we call this
  # function with tape_wrap which we re-evaluate on perturbed inputs, and add to l

  pert_inputs = tape_gather_alt_inputs(ent, l, promote)

  # if wrap is true then this will be the wrapped operation
  pert_output = do.call(ent$op, c(pert_inputs, ent$extra_args))
  l[[ent$id]] <- pert_output
  if(!identical(dim(pert_output),dim(ent$value))) {
    pv(dim(pert_output),dim(ent$value))
    stop("Error: cell_rerun_pert call to ",ent$op,
      " produced different dims than original")
  }
  l
}
# engine for dual computations.
# promote should unwrap inputs if wrap=F, and create a dual_number
# with zero dual around the array or tape_wrap object
cell_rerun_dual = function(ent, l, wrap=F) {
  # similar to add_pert_entry but with propagation of duals
  # l contains dual_number versions of each cell
  # use dual_op to construct from value and create dual_number from
  # this. also, need version storing/taking a list of multiple duals

  if(wrap) pro = dual_number
  else pro = dual_number %c% untapewrap

  dual_inputs = tape_gather_alt_inputs(ent, l, pro)
  if(!all(sapply(dual_inputs,is.dual_number))) {
    stop("Not all inputs are dual numbers: ",ent$id)
  }

  dual_op = dual_ops[[ent$op]]
  if(is.null(dual_op)) {
    stop("Dual operation not found for ",ent$op)
  }

  if(wrap) primal = ent
  else primal = ent$value

  # if wrap is true then the output will be a dual_number containing a
  # tape_wrap otherwise just a dual_number.
  dual_output = do.call(dual_op, c(list(primal), dual_inputs, ent$extra_args))

  l[[ent$id]] <- dual_number(primal, dual_output)

  l
}

is_just_list = function(l) {
  !is.numeric(l) && is.list(l)
}

# thoughts. for the purpose of perturbing the inputs, or for
# propagating a dual number, we need a function that can traverse the
# tape and update the values depending on x into a separate list. we
# need to think about how to do this generally so that there is only
# one traversal function. we can have the same function help us with
# the accumulators. start with the accumulator version and keep it
# general.

# forward_traverses
# xs: the tape_var (or list of tape_vars) whose descendents we're interested in
# ys: the final descendents we are traversing to
# xauxs: auxiliary values for xs, for e.g. tape_get_pert or tape_get_dual
# engine: function that adds cells to the fixup list during traversal
forward_traverses = function(xs, ys, xauxs=NULL,
  engine=cell_rerun_zero) {
  stop_if_no_tape()

  if(!is.null(xauxs) && !is_just_list(xauxs)) xauxs=list(xauxs)
  if(!is_just_list(xs)) xs=list(xs)
  if(!is_just_list(ys)) ys=list(ys)

  n = .tape$length
  l = list()

  y_inputs = find_all_inputs(ys)
  lapply(seq_along(xs), function(i) {
    x=xs[[i]]
    if(!identical(x,.tape$get(x$id))) {
      # using a different tape?
      stop("input ",i," doesn't match tape")
    }
    if(!x$id %in% y_inputs) {
      stop("no output is a descendent of input ",i)
    }
  })
  restrict_ids = y_inputs
  # sort ascending
  restrict_ids = sort(restrict_ids)

  have_cell_id = function(id) { list_exists(l,id) }

  if(!is.null(xauxs)) {
    stopifnot(length(xs)==length(xauxs))
    lapply(seq_along(xs), function(i) {
      x = xs[[i]]
      xaux = xauxs[[i]]
      stopifnot(identical(dim(x$value), dim(xaux)))
      l[[x$id]] <<- xaux
    })
  } else {
    # used with engine=cell_rerun_zero
    for(x in xs) {
      l = engine(x, l)
    }
  }

  # execute the computation
  for(i in restrict_ids) {
    ent = .tape$get(i)
    inputs = ent$inputs
#    message("forward_traverse: ",sv(i,inputs,ent$op))
    if(any(sapply(inputs, have_cell_id))) {
      l = engine(ent, l)
    }
  }
  return(l)
}

# rerun the computation with a different value for x
# x,y: tape_wrap objects
# xaux: the new value, must be a tape_wrap if wrap=T
# wrap: if the new computation should add to the tape, and produce
#   tape_wrap'ped output
# promote: optional function to promote tape objects, e.g.
#   dual_number, before they are combined with xaux's descendants
tape_get_pert = function(xs, ys, xauxs, wrap=F, promote=NULL) {
  stop_if_no_tape()
  # call forward_traverse

  # allow users to call with xs or ys as a single variable
  if(!is_just_list(xauxs)) xauxs=list(xauxs)
  if(!is_just_list(xs)) xs=list(xs)
  singleton_output=F
  if(!is_just_list(ys)) {
    ys=list(ys)
    singleton_output=T
  }

  stopifnot(length(xs)==length(xauxs))

  if(!is.null(promote)) { pro = promote }
  else if(!wrap) { pro = untapewrap }
  else { # wrap==T
    # FHE 05 May 2025 apparently only test_02_pert gets here
    stopifnot(all(sapply(xauxs,is.tape_wrap)))
    pro = identity;
  }

  l = forward_traverses(xs, ys, xauxs=xauxs,
    engine=Curry(cell_rerun_pert, promote=pro)
  )
  # output: collect the l entries for each y in ys
  res = lapply(ys, function(y) { l[[y$id]] })
  if(singleton_output) res[[1]]
  else res
}

# calculate JVP
# was tape_get_dual
# propagate dual numbers through an existing tape
# like tape_get_pert but only track dual components
# xdot is the new dual component of x
# if wrap=T then xdot should be a tape_wrap
tape_get_dual =
tape_get_jvp = function(xs, ys, xdots, wrap=F) {
  # adapted from tape_get_pert
  stop_if_no_tape()

  # allow users to call with xs or ys as a single variable
  if(!is_just_list(xdots)) xdots=list(xdots)
  if(!is_just_list(xs)) xs=list(xs)
  singleton_output=F
  if(!is_just_list(ys)) {
    ys=list(ys)
    singleton_output=T
  }

  # call forward_traverse
  if(!wrap) {
    xauxs = lapply(seq_along(xs),
      function(i) {
        dual_number(xs[[i]]$value, xdots[[i]])
      })
    ## xaux = dual_number(x$value, xdot)
  } else {
    xauxs = lapply(seq_along(xs),
      function(i) {
        stopifnot(is.tape_wrap(xdots[[i]]))
        dual_number(xs[[i]], xdots[[i]])
      })
    ## xaux = dual_number(x, xdot)
  }

  l = forward_traverses(xs, ys, xauxs=xauxs,
    engine=Curry(cell_rerun_dual, wrap=wrap)
  )
  res = lapply(ys, function(y) { l[[y$id]]$dual })
  if(singleton_output) res[[1]]
  else res
}

# ----------------------------------------------------------------
# tape_get_grad: gradient calculation with backprop
#   tape_get_grad(x,y) - x and y are tape_wrap's
#     x must be wrapping a numeric
#     y must be wrapping a scalar value
#   1. traverse tape and compile a list of descendents of x (up to y)
#      and initialize accumulators (one for each descendent)
#   2. work backwards from y, following inputs and accumulating
#      adjoints in entries of the accumulator list from 1, until you
#      get to x. if you find an input with no accumulator then we can
#      ignore it since it is not a descendent of x.
#   3. return the x accumulator
# assume that .tape points to the current tape

# Notes: 1. this algorithm may not be optimal for all-paths (?) but
# its running time is dwarfed by the actual computation
# 2. we have opted not to add entries to the tape when computing the
# gradient, that would be a simple modification but we're not sure we
# would use it

# arguments: wrap, if true then record the gradient calculation on the tape
tape_get_grad = function(xs,y,wrap=F) {
  stop_if_no_tape()
  stopifnot(length(y$value)==1)

  singleton_input=F
  if(!is_just_list(xs)) {
    xs=list(xs)
    singleton_input=T
  }

  # call forward_traverse
  accums = forward_traverses(xs, list(y),
    engine=Curry(cell_rerun_zero,wrap=wrap))
  if(!list_exists(accums,y$id)) {
    stop("Didn't find y in accumulator list")
  }

  y_adj = ones_like(y$value)
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
        args = c(list(adj_out, val), unwrapped_inputs, ent$extra_args)
      } else {
        stopifnot(is.tape_wrap(adj_out))
        args = c(list(adj_out, ent), input_ents, ent$extra_args)
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
          stopifnot(is.numeric(r))
          if(wrap) { stopifnot(is.tape_wrap(r)) }
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
  # accums[[x$id]]
  res = lapply(xs, function(x) { accums[[x$id]] })
  if(singleton_input) res[[1]]
  else res
}

# ----------------------------------------------------------------
# (OLDish) Note: to do hvp (hessian-vector product):
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
