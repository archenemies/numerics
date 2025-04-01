# FHE 08 Dec 2018 "print value" function (actually from much earlier?)
# usage:
## > a=2;b=3;pv(a,b)
## a=[1] 2, b=[1] 3

# FHE 01 Apr 2025 TODO: pv(x=1) should assign x in parent.frame() and print its value

pv0 = function(x) {
  res = paste0(capture.output(x),collapse="\n");
  if(length(grep("\n",res))) {
    res = paste0("\n",res);
  }
  message(deparse(substitute(x)),"=",res)
}

# FHE 24 Jan 2017
# string for value, what's printed by pv.
# split off from pv
sv = function(...) {
  # FHE 09 Jul 2021 ...() creates list? where documented?
  dots=substitute(...())
  strs=c();
  for(i in 1:length(dots)) {
    ex=dots[[i]];
    exstr=paste0(deparse(ex),collapse="")
#    exval=eval.parent(ex);
    exval=eval(ex,parent.frame());
    force(exval)

    # FHE 08 Aug 2024 use deparse
    res = paste0(deparse(exval),collapse="\n");
#    res = paste0(capture.output(exval),collapse="\n");

    ## res = paste0(capture.output(exval),collapse="\n");
    # treat multilines specially
    if(length(grep("\n",res))) {
      # add final newline
      res = paste0("\n",res);
      # add indentation
      res=gsub("\n","\n   ",res);
      # add a newline to the expression string
      exstr = paste0("\n",exstr);
      if(i<length(dots)) {
        res = paste0(res,",\n");
      }
    } else {
      if(i<length(dots)) {
        res = paste0(res,", ");
      }
    }
    strs[i]=paste0(exstr,"=",res)
  }
  attr(strs,"value")=exval;
  strs
}

# print value.
# new version handling multiple arguments
# FHE 09 Jul 2021 return the last value
pv = function(...) {
  call = sys.call();
  call[[1]]=as.name("sv")
  strs = eval.parent(call)
  message(strs,sep="");
  invisible(attr(strs,"value"));
}

# print value
pv2 = function(x) {
  res = paste0(capture.output(x),collapse="\n");
  cat(deparse(substitute(x)),"=",res,"\n")
}
