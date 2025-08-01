# FHE 06 Apr 2025

mysource("generic-ops.R")

dim_like = function(v,x) {
  if(length(v) != length(x)) {
    stop("length mismatch in dim_like: ",
      sv(length(v)), "!=",
      sv(length(x)))
  }
  if(is.null(dim(x)))
    as.vector(v)
  else
    array(v,dim(x))
}

# repeat arg as many times as needed to make its dimensions the same
# as model. we put arg first so we can be like 'rep'.
rep_like = function(arg, model, each=F, check=T) {
  L = length(model)
  l = length(arg)
  stopifnot((L %% l) == 0)
  reps = L/l
  # l==1 for zeros_like
  if(l>1 && check && !is.null(dim(model))) {
    pv(dim(model), reps, L, l)
    if(each) stopifnot(dim(model)[[1]]==reps)
    else stopifnot(tail(dim(model),1)==reps)
  }
  # rep is generic for tape_wrap etc.
  if(each) res = rep(arg,each=reps)
  else res = rep(arg,times=reps)

  dim_like(res,model)
}

zeros_like.default = function(x, zero=0) {
  rep_like(zero, x)
}
zeros_like <- function(...) { UseMethod("zeros_like") }

# BUG: the Curry version doesn't do method dispatch for some reason:
#ones_like = Curry(zeros_like, zero=1)
ones_like = function(x) {
  zeros_like(x, zero=1)
}

# FHE 06 Apr 2025 for backprop testing
# FHE 25 May 2025 moved from util.R, changed to use rnorm
rand_fill = function(n, type="norm", offset=0.5) {
  v = rnorm(n)
  switch(type,
    norm=v,
    avoid_zero = (abs(v)+offset)*sign(v),
    stop("Unknown type in rand_fill"))
}

# TODO: make generic?
rand_like = function(obj, ...) {
  v = rand_fill(length(obj), ...)
  if(is.null(dim(obj)))
    as.vector(v)
  else
    array(v,dim(obj))
}

rand_array = function(dims, ...) {
  v = rand_fill(prod(dims), ...)
  array(v,dims)
}

# we need this version of subscript assignment for the opname of
# "[<-", because the extra_args (...) must appear at the end.
# but we only overload "[<-".
subscr_assign = function(x, value, ...) {
  x[...] <- value
  x
}

if(0) {
  pv(rep_like(2, array(1:24, dim=4:2)))
}

# FHE 21 May 2025 (written with help from Grok)
# also https://stackoverflow.com/questions/65674226/deal-with-missing-values-in-ellipsis-arguments-in-r
# helper for subscripting in bbound
# not sure if this would work with pqR
# it depends on sys.call() returning empty names for the missing
# arguments
missing_to_NA <- function(...) {
  args = as.list(substitute(list(...))[-1])
  lapply(args, function(arg) {
    if (length(arg)==1 && arg == quote(expr = )) NA else eval(arg)
  })
}
