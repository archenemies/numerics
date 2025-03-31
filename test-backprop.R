# FHE 30 Mar 2025 split off from backprop.R
# testing functions for backprop implementation

mysource("backprop.R")

test_backprop = function() {
  tape_init()
  test_tape1()
  show_tape()
  pv(get_desc_list(x))
  pv(get_desc_list(x, xaux=1.01, type="pert"))
  stopifnot(tape_get_pert(x,1.02,qq)==30.2)
  stopifnot(tape_get_grad(x,qq)==10)
  message("Passed test_backprop")
}

test_backprop()
