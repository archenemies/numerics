# FHE 18 May 2025
mysource("bbind.R")
mysource("ops-common.R") # ones_like

test_bprep = function() {
  r=setNames(1:4, letters[1:4])
  stopifnot(dimblocks(bprep(r))[[1]][["d"]]$ixs == 4)
  pv(bprep(r))
  message("Passed test_bprep")
}

test_bbind = function() {
  v1 = array(1:3, dim = c(3,1))
  dimnames(v1) = list(c("x", "y", "z"), NULL)
#  v1 = bprep(v1)

  v2 = array(4:6, dim = c(3,1))
  dimnames(v2) = list(c("u", "v", "w"), NULL)
#  v2 = bprep(v2)

  u = bbind(.along = 1, a = v1, b = v2)

  stopifnot(identical(dimblocks(u)[[1]][["b"]]$ixs, 4:6))

  stopifnot(identical(bfetch(u, 1, "b"), 4:6L))
  stopifnot(identical(bfetch(u, 1, "b$v"), 5L))

  # check case where names is missing:
  stopifnot("b$2" %in% rownames(bbind(a=v1, b=ones_like(v1))))

  # check subscripting
  stopifnot(all(u["b",] == 4:6L))
  # same
#  stopifnot(all(u["b",] == array(4:6L, dim=c(3,1))))

  stopifnot(u["b$v",] == 5)

  # try no-name mode
  w = bbind(.along = 2, v1, v2)
  stopifnot(identical(dim(w),as.integer(c(3,2))))
  stopifnot(is.null(dimnames(w)[[2]]))
  stopifnot(identical(dimnames(w)[[1]],c("x","y","z")))

  message("Passed test_bbind")
  export(v1, v2, u, w)
}
if(mySourceLevel==0) {
  ## test_bprep()
 test_bbind()
}
