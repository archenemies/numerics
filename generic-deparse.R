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
