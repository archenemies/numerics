# FHE 30 Mar 2025 split off from dual-number.R

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
  check_dual_op("solve")(r)
}

test_dual_number1()
