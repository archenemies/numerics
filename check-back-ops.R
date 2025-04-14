# -*- my-source-delegate: "test-backprop.R" -*-
# FHE 05 Apr 2025
# adapt check_dual_op and check_vec_op to
# check_back_op
# which checks the correctness of the back_op operations

# example of testing backpropagation of an operation (+) with vector
# inputs and outputs
test_check_back_plus = function(dim=3) {
  message("In test_check_back_plus")
  use_tape(new_tape())
  tape_var(x = 1, y = 1)
  xo = tape_var(rand_array(dim))
  yo = tape_var(rand_array(dim))
  xc = rep_like(x, xo)*xo
  yc = rep_like(y, yo)*yo

  zc = xc + yc

  zo = tape_var(rand_array(dim(zc)))
  z = sum(zc*zo)

  export(x,y,z)
  show_tape()

  pv(tape_get_grad(x,z))
  pv(tape_get_grad(y,z))

  check_tape_grad_pert(y,z)
  check_tape_grad_pert(x,z)
}

test_check_back_subscr = function() {
  message("In test_check_back_subscr")
  use_tape(new_tape())
  setup_tape3()
  show_tape()
  pv(tape_get_grad(x,z))
  check_tape_grad_pert(x,z)
}

check_back_op = function(op="+", tol=1e-3) {
  op_func <- get(op, envir = .GlobalEnv)

  function(...) {
    message("In check_back_op(",op,")")
    args = list(...)

    make_arg_cells = function(val) {
      tape_var(x = 1)
      x0 = tape_var(val)
      x1 = tape_var(rand_array(dim(val)))
      xc = x0 + rep_like(x, x1)*x1
      list(x,xc)
    }

    use_tape(new_tape())

    arg_vars = lapply(args, make_arg_cells)
    op_args = lapply(arg_vars, Curry(getElement,name=2))

    zc = do.call(op_func, op_args)

    zo = tape_var(rand_array(dim(zc)))
    z = sum(zc*zo)

#    show_tape()

    for(i in seq_along(args)) {
      x = arg_vars[[i]][[1]]
      check_tape_grad_pert(x,z)
    }
  }
}

test_check_back_ops = function(dim=3) {
  x = rand_array(dim)
  y = rand_array(dim)
  check_back_op("+")(x,y)
  check_back_op("*")(x,y)
  check_back_op("-")(x,y)
  check_back_op("-")(x)
  check_back_op("/")(x,y)
  check_back_op("exp")(x)
  check_back_op("log")(abs(x))
}

