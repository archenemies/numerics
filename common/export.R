# FHE 30 Mar 2025 (written over a decade ago)
# assign a list of names in the global environment
# useful with testing
# (should assign in parent.frame instead?)

## export = function(x) {
##   assign(deparse(substitute(x)), x,
##          pos=.GlobalEnv);
## }

## f = function() { x = 2; export(x); }

# assign some variables in the context of the parent frame
export <- function(..., envir=parent.frame(2)) {
	dots <- substitute(...()) # list of symbols?
	dp <- sapply(dots, deparse) # this will be a list of strings
	names <- if (is.null(names(dots))) rep("", length(dots))
    else names(dots)

	names[names==""] <- dp[names==""]
	for(i in seq_along(dots)) {
      assign(names[i], eval(dots[[i]], parent.frame()),
             envir=envir)
	}	
}

test_export1 = function() {
  a=1; b=1;
  g = function() {
    f = function() {
      a=2;
      export(a,b=3);
    }
    f()
    pv(a,b)
    stopifnot((a==2) && (b==3))
  }
  g()
  pv(a,b)
  stopifnot((a==1) && (b==1))
}

if(mySourceLevel==0) {
  test_export1()
}
