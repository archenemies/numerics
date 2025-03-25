# FHE 25 Mar 2025
# List datatype with O(1) appends
# --> it seems that by_index has been fixed so this is no longer necessary?

# from https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1

expanding_list <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0
  double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }
  add <- function(val) {
    if(length == capacity) { double.size() }
    length <<- length + 1
    buffer[[length]] <<- val
  }
  as.list <- function() {
    buffer[0:length] # 0:length correctly handles length==0
  }
  get <- function(ix) {
    buffer[[ix]]
  }
  environment()
}

expanding_list_methods <- function(capacity = 10) {
  buffer <- vector('list', capacity)
  length <- 0
  methods <- list()
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    capacity <<- capacity * 2
  }
  methods$add <- function(val) {
    if(length == capacity) { methods$double.size() }
    length <<- length + 1
    buffer[[length]] <<- val
  }
  methods$as.list <- function() {
    b <- buffer[0:length] # 0:length correctly handles length==0
    return(b)
  }
  methods
}

if(1) {
  library(microbenchmark)
  runBenchmark <- function(n) {
    microbenchmark(times = 5,
      c_ = {
        a <- list(0)
        for(i in 1:n) {a = c(a, list(i))}
      },
      list_ = {
        a <- list(0)
        for(i in 1:n) {a <- list(a, list(i))}
      },
      by_index = {
        a <- list(0)
        for(i in 1:n) {a[length(a) + 1] <- i}
        a
      },
      expanding_list = {
        a <- expanding_list()
        for(i in 1:n) { a$add(i) }
        a$as.list()
      },
      expanding_list_methods = {
        a <- expanding_list_methods()
        for(i in 1:n) { a$add(i) }
        a$as.list()
      }
    )
  }
}
