
## export = function(x) {
##   assign(deparse(substitute(x)), x,
##          pos=.GlobalEnv);
## }

## f = function() { x = 2; export(x); }

export <- function(...) {
	dots <- substitute(...()) # list of symbols?
	dp <- sapply(dots, deparse) # this will be a list of strings
	names <- if (is.null(names(dots))) rep("", length(dots))
    else names(dots)

    ## pv0(dots);
    ## pv0(names(dots));
    ## pv0(names);
    ## pv0(dp);
	## pv0(names[names==""]);
    ## pv0(dp[names==""]);
	names[names==""] <- dp[names==""]
	for(i in seq_along(dots)) {
      assign(names[i], eval(dots[[i]], parent.frame()),
#             envir=parent.frame(2))
             envir=.GlobalEnv)
	}	
}

# FHE 13 Oct 2024
# XXX update above function to call this helper?
export_names <- function(..., envir=.GlobalEnv) {
  names = list(...)
  pv(names)
  for(i in seq_along(names)) {
    assign(names[[i]], get(names[[i]], envir=parent.frame()),
      envir=envir)
  }	
}

# should rewrite the above function to use this helper
## > f=function () {a=2; export_str("a");}; f(); a
## [1] 2
export_str = function(l) {
  lapply(l,function(elt) {
      assign(elt, eval(parse(text=elt), parent.frame(3)),
        envir=.GlobalEnv)
  })
  invisible(l)
}

# evaluate an expression, exporting any new variables that were
# created to the global environment (opposite of "within")
export_new = function(expr) {
  eval(substitute(
    {
      .vars=ls();
      .res=expr;
      export_str(setdiff(ls(),c(.vars,".vars",".res")));
      .res
    }
  ), envir=parent.frame(1))
}


test_export = function () {
  givevals <- function() {
	x <- 10
	y <- 30
	export(x, z=y)
	
  }

  hello<-function() {
	givevals()
	print(x)	
	print(z)	
  }
  hello()
}
