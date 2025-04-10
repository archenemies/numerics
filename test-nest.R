# FHE 10 Apr 2025

mysource("backprop.R")
mysource("dual-number.R")
mysource("test-dual-number.R")

# adapted from test_dual_array
nest_fn = function(x) {
    y = sum(x[3:5]*x[1:3])
    z = y*y
    z
}

# dual_number within tape_wrap
test_nest1 = function() {
  xd = dual_number(
    rand_array(10),
    rand_array(10)
  )
  check_dual_function(nest_fn, list(xd))

  use_tape(new_tape())
  xdtv = tape_var(xd)
  zdtv = nest_fn(xdtv)
  show_tape()
  pv(tape_get_pert(xdtv,zdtv,xd))
  pv(tape_get_grad(xdtv,zdtv))
}

# tape_wrap within dual_number
test_nest2 = function() {
  tape_var(tv_primal = rand_array(10),
    tv_dual = rand_array(10))
  xtvd = dual_number(tv_primal, tv_dual)
  ztvd = nest_fn(xtvd)
  pv(ztvd)
  show_tape()
}

if(mySourceLevel==0) {
  test_nest2()
}
