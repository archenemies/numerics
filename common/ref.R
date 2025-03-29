# FHE 3 feb 2020, from a stackexchange discussion

# a ref is just an environment with one thing in it. you can create a
# ref to a variable in the current environment using ref(v). or an
# anonymous ref to a variable "val" in a new environment using
# r=ref_to(77)
# getRef(r) -> 77

# FHE 06 Jul 2021
mysource("util.R")

newRef = function(name,env) {
  r=list(name=name, env=env);
  class(r) <- "ref";
  r
}

ref <- function(v) {
  arg <- deparse(substitute(v))
  newRef(arg,where(arg, env=parent.frame()))
}

# FHE 07 Jul 2021 create an anonymous reference
ref_to = function(val) {
  env=new.env();
  env$val=val;
  newRef("val",env);
}

getRef <- function(r) {
  get(r$name, envir = r$env, inherits = FALSE)
}

setRef <- function(r, x) {
  assign(r$name, x, envir = r$env)
}

# FHE 07 Jul 2021 added a print method
print.ref = function(x, ...) {
  cat("<ref \"",x$name,"\", ",format(x$env),">\n",sep="");
  invisible(x);
}

if(0) { # tests
  v <- 1:5
  r <- ref(v)
  (function() {
    stopifnot(identical(getRef(r),1:5))
    setRef(r, 1:6)
  })()
  stopifnot(identical(v,1:6))

  # this refers to v in the global environment
  v=2; r=(function() {ref(v)})()
  stopifnot(getRef(r)==2)
  setRef(r,5)
  stopifnot(getRef(r)==5)
  stopifnot(v==5)

  # same as above
  v=2; r=(function() {v <<- 3; ref(v)})()
  stopifnot(getRef(r)==3)
  setRef(r,5)
  stopifnot(getRef(r)==5)
  stopifnot(v==5)

  # this creates a local binding first, and refers to that
  v=2; r=(function() {v=3; ref(v)})()
  stopifnot(getRef(r)==3)
  setRef(r,5)
  stopifnot(getRef(r)==5)
  stopifnot(v==2)

  # additional tests
  r=(function() {v=4; (function(v1) { ref(v1) })(v)})()
  stopifnot(r$name=="v1")
  stopifnot(getRef(r)==4)
  setRef(r,5)
  stopifnot(getRef(r)==5)

  # check that outer v is not modified
  v=2; r=(function() {(function(v1) { ref(v1) })(v)})()
  stopifnot(getRef(r)==2)
  setRef(r,5)
  stopifnot(getRef(r)==5)
  stopifnot(v==2)

  v=77; r=ref_to(v)
  stopifnot(getRef(r)==77);
}
