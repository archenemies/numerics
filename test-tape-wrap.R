# FHE 30 Mar 2025 split off from tape-wrap.R
# testing functions for tape_wrap and tape

mysource("tape-wrap.R")
mysource("export.R")

setup_tape1 = function() {
  # simple tape
  # compute (1+2)*2*5
  y <- tape_var(2); # try different uses of tape_var
  pv(y)
  tape_var(x=1, v=5);
  pv(x,v)
  z <- x+y
  pv(z)
  w <- z*tape_var(2)
  qq <- w*v
  pv(qq)
  export(x,y,z,w,v,qq)
}

setup_tape2 = function() {
  # a slightly more complicated tape
  # y depends on x via separate paths
  tape_var(x=2, u=5);
  z <- (x+u)*tape_var(2)
  w <- x+tape_var(3)
  y <- z*w
  pv(x,u,w,z,y)
  export(x,u,w,z,y)
  # y= 2*(x+5)*(x+3)
  # dy/dx = 2*(2*x+8) = 24
  # d^2y/dx^2 = 4
}

test_tape1 = function() {
  tp = new_tape()
  use_tape(tp)
  setup_tape1()
  show_tape(tp)
  use_tape(NULL) # optional
  free_tape(tp)
}

test_tape2 = function() {
  tp = new_tape()
  use_tape(tp)
  setup_tape2()
  show_tape(tp)
  free_tape(tp)
}
