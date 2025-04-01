# FHE 30 Mar 2025 split off from backprop.R
# testing functions for backprop implementation

mysource("backprop.R")

mysource("test-tape-wrap.R")

test_01_pert = function() {
  message("In test_01_pert")
  use_tape(new_tape())
  setup_tape1(export=TRUE)
  show_tape()
  pv(forward_traverse(x))
  pv(forward_traverse(x, xaux=1.01, type="pert"))
  stopifnot(tape_get_pert(x,1.02,qq)==30.2)
  stopifnot(tape_get_grad(x,qq)==10)
  message("Passed test_pert")
}

test_02_pert = function() {
  message("In test_02_pert")
  use_tape(new_tape())
  setup_tape2(export=TRUE)
  show_tape()

  # XXX check values
  # XXX figure out what we are doing here
  pv(forward_traverse(x))
  pv(forward_traverse(x, xaux=2.01, type="pert"))
  pv(tape_get_pert(x,2.01,y))
  pv(tape_get_grad(x,y))
  message("Passed test_02_pert")
}

