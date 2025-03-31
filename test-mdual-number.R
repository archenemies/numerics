# FHE 30 Mar 2025 split off from mdual-number.R

mysource("mdual-number.R")

if(0) {
  # tests:
  x = mdual_number(1, array(1), "dt")
  promote_mdual(x, c("du","dt"))

  arr = array(1:24, c(2,3,4))
  ax = mdual_number(arr, vector_of(arr), "dt")
  promote_mdual(ax, c("dt","du"))

  y = mdual_number(4, array(1), "du")
  s = sync_mduals(list(x,y))
}

