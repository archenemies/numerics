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

if(mySourceLevel==0) {
  ## test_dual_number1()
  ## test_dual_ops()
  ## test_dual_ops(dim=c(3,2,4))
  test_dual_array()
}
