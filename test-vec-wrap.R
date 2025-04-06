# FHE 05 Apr 2025

mysource("vec-wrap.R")

test_vec_wrap1 = function() {
  a=array(1:24,dim=2:4)
  x=vec_wrap(a)
  pv(dim(x))
  stopifnot(identical(dim(x),2:3))
  b=x*x
  pv(b)
  d = pv(vec_wrap_of(4)(2))
  pv(d)
  pv(b^(-b))
  export(a,x,b,d)
}

check_vec_op = function(op, tol=1e-5) {
  # from check_dual_ops. return function of (...) that takes
  # vec_wrapped arguments and applies op to them, and then applies the
  # same op in a loop to the argument values and checks that the
  # result is the same
  op_func <- get(op, envir = .GlobalEnv)
  function(...) {
    args = list(...);
#    unwrapped_args = lapply(args, unvecwrap)
    arg_dims = dim(args[[1]]$value)
    for(i in seq_along(args)) {
      idims = dim(args[[i]]$value)
      if(!identical(arg_dims, idims)) {
        stop("Wrong argument dimensions in check_vec_op: ",
          deparse(arg_dims), "!=",deparse(idims))
      }
    }
    vec_res = do.call(op_func, args)
    n = tail(arg_dims,1)
    lapply(1 %upto% n, function(i) {
      # get ith element of each arg vector
      args_subsc_i = lapply(args, function(x) {
        lastInd(x$value, i)
      })
      # call op
      i_res = do.call(op_func, args_subsc_i)
      # check that the result is same as subsetting vec_res$value
      vec_res_i = lastInd(vec_res$value,i);
#      pv(i,vec_res_i,i_res)
      stopifnot(sum(abs(i_res-vec_res_i)) < tol)
    })
    message("  Passed ",op)
    invisible(TRUE)
  }
}

test_vec_wrap_ops = function(dim=3, fill="seq") {
  message("Testing vec_wrap operations, dim=",deparse(dim))
  n = prod(dim)
  genfill = function(n) {
    switch(fill,
      rnorm = rnorm(n),
      seq = 1:n,
      stop("Unknown fill type ",fill))
  }
  x = vec_wrap(array(genfill(n), dim=dim))
  y = vec_wrap(array(rev(genfill(n)), dim=dim))

  check_vec_op("+")(x,y)
  check_vec_op("-")(x,y)
  check_vec_op("*")(x,y)
  check_vec_op("sum")(x)
  message("Passed all operations")
}

if(mySourceLevel==0) {
#  test_vec_wrap1()
  test_vec_wrap_ops()
  test_vec_wrap_ops(dim=c(2,3), fill="rnorm")
}
