# FHE 05 Apr 2025
# adapt check_dual_op and check_vec_op to
# check_back_op
# which checks the correctness of the back_op operations

# XXX call this with different dimension numbers
# XXX generalize it to take an op argument
test_check_back_op = function(dim=3) {
  message("In test_check_back_op")
  use_tape(new_tape())
  n = prod(dim)
  make_obj = function() {
    array(rand_fill(n), dim=dim)
  }
  tape_var(x = 1, y = 1)
  xo = tape_var(make_obj())
  yo = tape_var(make_obj())
  xc = rep_like(x, xo)*xo
  yc = rep_like(y, yo)*yo

  zc = tape_var(make_obj())
  z = sum((xc + yc)*zc)

  export(x,y,z)
  show_tape()
  pv(tape_get_grad(x,z))
  pv(tape_get_grad(y,z))
  check_tape_grad_pert(y,z)
  check_tape_grad_pert(x,z)
}

if(mySourceLevel==0) {
  test_check_back_op()
}

