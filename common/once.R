# FHE 13 Dec 2016
# implement simplest form of caching

# assign a value to a variable, once. do it in the global scope by
# default. if the variable 'exists()' then don't execute the
# expression. check if input is like l$x, in which case use 'names'
# rather than 'exists'

# FHE 05 Jul 2021
if(!exists(".once_be_quiet")) .once_be_quiet <<- T;

once = function(var,value,envir=globalenv()) {
  msg=function(...) {if(!.once_be_quiet) message(...)}
#  msg=function(...) {}
  vn=substitute(var);
  ex=switch(
    class(vn),
    name={
#      msg("once: Called with a variable ",vn)
      exists(as.character(vn))
    },
    call={
      msg("once: Called with a call ",paste(as.list(vn)))
      if(vn[[1]]==as.name("$")) {
        vn2=as.character(vn[[2]])
        if(!exists(vn2,1)) {
          stop("Trying to process call ",deparse(vn)," but found no variable ",vn2);
        }
        l<-eval.parent(n=1,vn[[2]])
        as.character(vn[[3]]) %in% names(l)
      } else {
        stop("Error, unrecognized call (expected list$name): ",deparse(vn));
      }
    },
    stop("Error, expression is not a variable or a list$name: ",deparse(vn))
  )
  if(ex) {
    msg("once: not overwriting ",deparse(vn));
  } else {
    # FHE 02 Nov 2021 what was this for?
#    tryCatch(eval.parent(n=,substitute(value)), error=function(e) { stop("once: Couldn't evaluate ",deparse(substitute(value)),": ",e,call.=F)})
    msg("once: Setting ",deparse(vn)," to ",deparse(substitute(value)))
    ca=call("<-",vn,value);
    eval(ca,env=envir);
  }
  invisible(eval.parent(n=1,vn))
}
`%<-1%`=once

# it doesn't make sense to have an "inherits" option to this function,
# a la "assign", because we assume that the first time it is called
# the variable won't exist anywhere, so we always have to choose an
# environment to put it in (and to expect it to to be in, next time
# around)

# TODO:
# - version which looks at text of "value" and checksums it
# - possibly, more sophisticated versions
