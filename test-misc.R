# FHE 21 May 2025
mysource("ops-common.R")

test_missing_to_NA = function() {
  f = function() {
    message("in f") # should only be printed once
    2
  }
  a=f()
  l = missing_to_NA(1,,a)
  pv(l)
  stopifnot(is.na(l[[2]]))
  # missing_to_NA(1, , a)=list(1, NA, 2)

  # test using it in another function
  g = function(...) {
    missing_to_NA(...)
  }
  l = g(,2,)
  pv(l)
  stopifnot(all(is.na(l[c(1,3)])))
  export(l)
  
  message("passed test_missing_to_NA")
}

test_missing_to_NA()
