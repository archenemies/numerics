# FHE 25 Mar 2025
# List datatype with O(1) appends

# from https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1

expanding_list <- function(capacity = 10) {
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
