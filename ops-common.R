# FHE 06 Apr 2025

mysource("generic-ops.R")

dim_like = function(v,x) {
  if(is.null(dim(x)))
    as.vector(v)
  else
    array(v,dim(x))
}

# repeat arg as many times as needed to make its dimensions the same
# as model. we put arg first so we can be like 'rep'.
rep_like = function(arg, model) {
  L = length(model)
  l = length(arg)
  stopifnot((L %% l) == 0)
  reps = L/l
  # rep is generic for tape_wrap etc.
  res = rep(arg,reps)

  dim_like(res,model)
}

if(0) {
  pv(rep_like(2, array(1:24, dim=4:2)))
}
