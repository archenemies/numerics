# FHE 30 Mar 2025 allow pretty deparse for our classes

# https://adv-r.hadley.nz/s3.html#s3-classes
## my_new_generic <- function(x) {
##   UseMethod("my_new_generic")
## }

deparse.default <- function(...) {
  base::deparse(...)
}

deparse <- function(...) {
  UseMethod("deparse")
}

# our deparse for numerics. shorten numbers to 4 decimal places. since
# base::deparse is doing complicated stuff like deciding when to write
# "c()" and when to write "1:4", it seems easier to just manipulate
# the string it returns than to try to duplicate its functionality.
deparse.numeric = function(x,...) {
    str = base::deparse(x,...);
    gsub("(\\.\\d\\d\\d\\d)\\d+","\\1", str)
}

# from R-4.2.0/src/library/base/R/utilities.R
## deparse(.) returning *one* string
deparse1 <- function(expr, collapse = " ", width.cutoff = 500L, ...)
    paste(deparse(expr, width.cutoff, ...), collapse=collapse)

