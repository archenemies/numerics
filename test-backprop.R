# FHE 30 Mar 2025 split off from backprop.R
# testing functions for backprop implementation

mysource("backprop.R")
mysource("tape-wrap.R")

mysource("test-tape-wrap.R") # for setup_tape*()
mysource("check-dual-ops.R") # for check_dual_function

test_01_pert = function() {
  message("In test_01_pert")
  use_tape(new_tape())
  setup_tape1()
  show_tape()
  ## pv(forward_traverse(x))
  ## pv(forward_traverse(x, xaux=1.01, type="pert"))
  stopifnot(tape_get_pert(x,qq,1.02)==30.2)
  stopifnot(tape_get_grad(x,qq)==10)
  check_tape_grad_pert(x,qq)
  message("Passed test_pert")
}

# TODO: generalize to vector x?
# or just always use random vectors to test
tape_pert_numdiff = function(x,y,h=1e-4) {
  xp = x$value + h;
  yp = tape_get_pert(x, y, xp)
  (yp-y$value)/h
}
check_tape_grad_pert = function(x,y,tol=1e-2) {
  message("check_tape_grad_pert: checking derivative of ",sv(y)," with respect to ",sv(x))
  dp = tape_pert_numdiff(x,y)
  dg = tape_get_grad(x,y)
  pv(dp,dg)
  if(abs(dp-dg)<tol) {
    message("passed");
  } else {
    stop("failed: ",dp," != ",dg)
  }
  dg
}

test_02_pert = function() {
  # test type="pert" forward_traverse with tape 2
  # also check wrap=T

  use_tape(new_tape())
  setup_tape2()
  stopifnot(.tape$length==8)
  pv(tape_get_grad(x,y))
  check_tape_grad_pert(x,y)

  # now check that wrapped pert has the same grad
  y_inputs = find_all_inputs(y)
  tape_var(x1=x$value)
  ## l = forward_traverse(x, xaux=x1, type="pert", restrict_ids=y_inputs,
  ##   wrap=T)
  ## y1 = l[[y$id]]
  y1 = tape_get_pert(x, y, x1, wrap=T)
  stopifnot(is.tape_wrap(y1))
  pv(tape_get_grad(x1,y1))
  stopifnot(.tape$length == 13)

  message("Passed test_02_pert")
}

test_03_grad_wrap = function() {
  # use tape 2, calculate a wrapped gradient
  # so that we can get second derivative
  use_tape(new_tape())
  setup_tape2()
  stopifnot(.tape$length==8)
#  pv(tape_get_grad(x,y))

  show_tape()
  g=pv(tape_get_grad(x,y,wrap=T))
  show_tape()
  g2=pv(tape_get_grad(x,g))
  stopifnot(g2==4)
  message("Passed test_03_grad_wrap")
}

test_jvp = function() {
  use_tape(new_tape())
  setup_tape4()
  xdot = rand_like(x)
  pv(z)
  pv(tape_get_jvp(x,z,xdot))
  jvp_fn = function(xdual) {
    if(is.dual_number(xdual)) {
      dual_number(z$value, tape_get_jvp(x,z,xdual$dual))
    } else {
      tape_get_pert(x,z,xdual)
    }
  }
  check_dual_function(jvp_fn, list(dual_number(x$value, xdot)))
}

test_jvp_wrap = function() {
  tol = 1e-4
  use_tape(new_tape())
  setup_tape4()
  tape_var(xdot = rand_like(x))

  zdot = tape_get_jvp(x,z,xdot,wrap=T)

  .xdot1 = rand_like(x)

  .zdot1.pert = tape_get_pert(xdot,zdot,.xdot1)
  .zdot1.jvp = tape_get_jvp(x,z,.xdot1)

  dif = sum(abs(.zdot1.pert - .zdot1.jvp))
  if(dif > tol) {
    stop("Failed test_jvp_wrap: ",sv(dif,tol))
  }
  message("Passed test_jvp_wrap")
}

test_rep = function() {
  use_tape(new_tape())

  tape_var(r = 1)
  tape_var(x0 = rand_array(3))
  x = rep_like(r, x0) * x0
  tape_var(y = rand_array(c(3,2)))
  z = sum(y*(rep_like(x, y)))
  export(r,x,y,z)

  check_tape_grad_pert(r,z)
}

test_rep_each = function() {
  use_tape(new_tape())

  tape_var(r = 1)
  tape_var(x0 = rand_array(3))
  x = rep_like(r, x0) * x0
  tape_var(y = rand_array(c(2,3)))
  z = sum(y*(rep_like(x, y, each=T)))
  export(r,x,y,z)

  check_tape_grad_pert(r,z)
}

test_back_c = function() {
  message("in test_back_c")
  use_tape(new_tape())

  tape_var(r = 1)
  tape_var(x0 = rand_array(2))
  tape_var(x1 = rand_array(4))
  x = c(x0*rep_like(r, x0),x1*rep_like(r, x1))
  pv(dim(x))
  tape_var(y = rand_array(c(6)))
  pv(dim(y))
  z = sum(y*dim_like(x,y))
  export(r,x,y,z)

  check_tape_grad_pert(r,z)
}

mysource("check-back-ops.R")

if(mySourceLevel==0) {
  mysource("test-backprop.R")
#  test_01_pert()
#  test_03_grad_wrap()
#  test_check_back_plus()
#  test_check_back_ops()
#  test_check_back_subscr()
#  test_jvp()
#   test_jvp_wrap()
#  test_rep()
#  test_rep_each()
  test_back_c()
}
