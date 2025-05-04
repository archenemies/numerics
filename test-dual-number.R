# FHE 30 Mar 2025 split off from dual-number.R

mysource("dual-number.R")

mysource("check-dual-ops.R")

test_dual_number1 = function() { # testing
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
  check_dual_op("solve",tol=1e-3)(r)
  export(x,y,z,r)
}

# a more organized and vector-valued version of the above
test_dual_ops = function(dim=3, tol=1e-4) {
  message("Testing dual_number operations, dim=",deparse(dim))
  n = prod(dim)
  make_dual = function() {
    dual_number(array(rand_fill(n), dim=dim),
      array(rand_fill(n), dim=dim))
  }
  x = make_dual()
  y = make_dual()

  cdo = Curry(check_dual_op, tol=tol)

  cdo("+")(x,y)
  cdo("/")(x,y)
  cdo("-")(x,y)
  cdo("*")(x,y)
  cdo("sum")(x)
  cdo("log")(x*x)
  cdo("exp")(x)

  if(length(dim)==1) {
    make_dual_matrix = function() {
      dual_number(
        matrix(rand_fill(n*n), nrow = n),
        matrix(rand_fill(n*n), nrow = n)
      )
    }
    r = make_dual_matrix()
    u = make_dual_matrix()

    cdo("t")(r)
    cdo("%*%")(r,u)
    cdo("solve",tol=tol*10)(r)
    cdo("rowSums")(r)
    cdo("colSums")(r)
    export(r,u)
  }

  message("Passed all operations")
}

test_dual_array = function() {
  # TODO we can also use this function with backprop
  array_fn = function(x) {
    y = sum(x[3:5]*x[1:3])
    z = y*y
    z
  }
  array_obj = dual_number(
    rand_array(10),
    rand_array(10)
  )
  check_dual_function(array_fn, list(array_obj))
}

test_dual_c = function() {
  c_fn = function(x) {
    sum(c(x,x)*c(
      zeros_like(x,zero=2),
      zeros_like(x,zero=1)))
  }
  c_obj = dual_number(
    rand_array(10),
    rand_array(10)
  )
  check_dual_function(c_fn, list(c_obj))
}

test_dual_dual = function(tol=1e-5) {
  xv = 5
  x = dual_number(
    dual_number(xv, 1),
    dual_number(1, 0)
  )

  q = x + ones_like(x)
#  y = (x+dual_number(dual_number(1)))
  y = q*q*x

  pv(abs(y$dual$value - y$value$dual))
  stopifnot(abs(y$dual$value - y$value$dual) < tol)

  pv(y$dual$dual)
  # 2nd derivative is 6x+4, = 34
  stopifnot(abs(y$dual$dual - 34) < tol)

  message("Passed test_dual_dual")
}

# FHE 24 Apr 2025 from test_dual_dual
test_tape_dual_dual = function(tol=1e-5) {
  mysource("backprop.R")
  xv = 5

  fn = function(x) {
    q = x + ones_like(x)
    y = q*q*x
  }

  yv = fn(xv)
  ydv = fn(dual_var(xv))
  yddv = fn(dual_var(dual_var(xv)))

  stopifnot(abs(ydv$value-yv)<tol)
  stopifnot(abs(ydv$dual-yddv$value$dual)<tol)
  stopifnot(abs(ydv$dual-yddv$dual$value)<tol)

  use_tape(new_tape())
  tape_var(xtv = xv)
  ytv = fn(xtv)
  grad1 = tape_get_grad(xtv, ytv, wrap=T)
  pv(grad1)
  # FHE 24 Apr 2025 was wrap=T but wanted shorter tape
  grad2 = tape_get_grad(xtv, grad1, wrap=F)
  pv(grad2)
  stopifnot(abs(yddv$dual$dual - grad2) < tol)

  xdot = ones_like(xtv)
  ydtv = tape_get_dual(xtv, ytv, xdot, wrap=T)
  pv(ydtv)

  stopifnot(abs(ydtv$value-grad1$value) < tol)

  # a bit hard to understand, but this calculates second derivative
  # using dual ops
  yddtv = tape_get_dual(xtv, ydtv, xdot, wrap=T)
  pv(yddtv)

  stopifnot(abs(yddtv$value-grad2) < tol)

  message("Passed test_tape_dual_dual")
}

if(mySourceLevel==0) {
  ## test_dual_number1()
  ## test_dual_ops()
  ## test_dual_ops(dim=c(3,2,4))
  ## test_dual_dual()
  ## test_tape_dual_dual()
  test_dual_c()
}
