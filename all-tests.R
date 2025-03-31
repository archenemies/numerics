# FHE 30 Mar 2025
# source the various test files and run all the tests

# unit tests can export variables to global environment, but should
# work independently of the other tests. we source(local=TRUE) to get
# the names of the tests defined by each file; this doesn't enforce
# independence. 'export' should probably be changed to export to the
# parent frame. XXX

# -> TEST_FILTER environment variable also specifies a regex to match
# test names against

source_tests = function(fn) {
  filt = Sys.getenv("TEST_FILTER")
  message("Sourcing test batch: ",fn);
  source(local=TRUE, fn);
  tests = grep("^test_",ls(),value=T)
  for(i in seq_along(tests)) {
    tn = tests[[i]]
    if(filt!="") {
      enabled = grepl(filt, tests[[i]])
    } else {
      enabled = TRUE
    }
    if(enabled) {
      message("Running test ",i,": ",tn)
      do.call(tn,list())
      message("Passed test ",i,": ",tn)
    } else {
      message("Eliding test ",i,": ",tn," (TEST_FILTER=",filt,")")
    }
  }
}

source_tests("test-tape-wrap.R")
source_tests("test-dual-number.R")
source_tests("test-backprop.R")
