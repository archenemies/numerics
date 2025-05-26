mysource("pv.R");

`%||%` = function(a,b) if(!is.null(a)) a else b

is.true = function(x) { length(x) > 0 && x }

# compose operator
`%c%` = function(x,y)function(...)x(y(...))

# FHE 06 Apr 2025
# override any duplicate names
# with last entry
# with help from grok (to find "duplicated()")
override_dups = function(l) {
  if (!is.list(l)) stop("Input must be a list")
  na = names(l)
  n = length(l)
  keep = (na=="") | !rev(duplicated(rev(na)))
  l[keep]
}
# original version:
## override_dups = function(l) {
##   na = names(l)
##   seen = list()
##   res = list();
##   for(i in rev(seq_along(l))) {
##     ni = na[i]
##     if(ni!="") {
##       if(!is.null(seen[[ni]])) {
##         next
##       }
##       seen[[ni]] = T
##     }
##     res = c(l[i], res)
##   }
##   res
## }

# currying (partial application)
Curry <- function(FUN,...) {
  .orig = list(...);
  # FHE 06 Apr 2025
  # we want to be able to use Curry to specify new defaults
  function(...) do.call(FUN,override_dups(c(.orig,list(...))))
  ## function(...) do.call(FUN,c(.orig,list(...)))
}

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

# FHE 08 Apr 2025 orphan helper
# fix problem with tail(x,-0)
tail_minus = function(x, n) {
  if(n==0) x
  else tail(x,-n)
}

