# FHE 30 Mar 2025 split off from tape-wrap.R
# testing functions for tape_wrap and tape

mysource("tape-wrap.R")
mysource("export.R")

setup_tape1 = function(export=F) {
  # compute (1+2)*2*5
  y <- tape_var(2);
  pv(y)
  tape_var(x=1, v=5);
  pv(x,v)
  z <- x+y
  pv(z)
  w <- z*tape_var(2)
  qq <- w*v
  pv(qq)
  if(export)
    export(x,y,z,w,v,qq)
}

test_tape1 = function() {
  tape_init()
  setup_tape1()
  show_tape()
}
