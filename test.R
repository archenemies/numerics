func <- function(...) {
  args <- list(...);
  arg_names = lapply(substitute(...()),deparse)
  lapply(seq_along(args), function(i) {
    a = args[[i]]
    n = arg_names[[i]]
    pv(a,n)
  })
}
