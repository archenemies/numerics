# FHE 30 Mar 2025 split off from backprop.R
# testing functions for backprop implementation

mysource("backprop.R")

mysource("test-tape-wrap.R")

test_01_pert = function() {
  message("In test_01_pert")
  tape_init()
  setup_tape1(export=TRUE)
  show_tape()
  pv(get_desc_list(x))
  pv(get_desc_list(x, xaux=1.01, type="pert"))
  stopifnot(tape_get_pert(x,1.02,qq)==30.2)
  stopifnot(tape_get_grad(x,qq)==10)
  message("Passed test_pert")
}
