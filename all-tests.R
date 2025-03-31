# FHE 30 Mar 2025
# source the various test files and run all the tests

# unit tests can export variables to global environment, but should
# work independently of the other tests. we source(local=TRUE) to get
# the names of the tests defined by each file; this doesn't enforce
# independence. 'export' should probably be changed to export to the
# parent frame. XXX

source_tests = function(fn) {
  message("Running test batch: ",fn);
  source(local=TRUE, fn);
  tests = grep("^test_",ls(),value=T)
  for(i in seq_along(tests)) {
    message("Test ",i,": ",tests[[i]])
    do.call(tests[[i]],list())
    message("Passed test ",i,": ",tests[[i]])
  }
}

source_tests("test-tape-wrap.R")
source_tests("test-dual-number.R")
source_tests("test-backprop.R")
