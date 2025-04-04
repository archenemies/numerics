mysource("pv.R");

`%||%` = function(a,b) if(!is.null(a)) a else b

is.true = function(x) { length(x) > 0 && x }

gmean = function(x) {
  exp(mean(log(x)))
}

`%upto%` = function (from, to)
  if (from <= to) from:to else numeric(0)

inRange=function(v,r) {
  stopifnot(length(r)==2);
  return(v>r[1] & v<r[2]);
}

# FHE 06 Jul 2021 moved from ref.R, for swap
# copy/modified from pryr::where
where = function(name, env=parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  }
  if (exists(name, env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))
  }
}

# FHE 02 Jul 2021 goes with 'where'. only works with simple variable
# names for now
assign_nearest = function(x, value, env=parent.frame()) {
  e=where(x,env=env);
  assign(x,value,envir=e)
}

# FHE 02 Jul 2021 modified greatly from seqinr
swap = function(x, y) {
  sx=deparse(substitute(x)); sy=deparse(substitute(y));
  t=x;
  assign_nearest(sx, y, env=parent.frame());
  assign_nearest(sy, t, env=parent.frame());
}

# FHE 06 Jul 2021 a shorthand for list(a=a,b=b)
# a=2;b=1;list_vars(a,b)
list_vars=function(...) {
  exprs=substitute(...())
  ns=as.character(exprs);
  args=as.list(exprs);
  names(args)=ns
  do.call(list,args,envir=parent.frame())
}

# FHE 28 Jan 2022
sigmoid=function(x,r) {
  s=x/r
#  exp(s)/(exp(-s)+exp(s))
  1/(1+exp(-2*s))
}
if(0) {
 xs = seq(-10,10,0.1)
 plot(xs,sigmoid(xs,4))
}

# FHE 08 Aug 2024 catd = cat deparse
catd = function(expr) { cat(deparse1(expr),"\n") }

# helper function to append 1 to dimension list
vector_of = function(x) {
  dim(x) = c(dim(x),1)
  x
}

