# FHE 06 Apr 2025
# create generic methods

mysource("generic-deparse.R")

# example generic method pair:
## log.default <- function(...) { base::log(...) }
## log <- function(...) { UseMethod("log") }

convertToGeneric <- function(fname) {
  base_fn = getFromNamespace(fname, "base")
  assign(paste0(fname, ".default"),
         base_fn,
         envir = .GlobalEnv)
  assign(fname,
         function(...) UseMethod(fname),
         envir = .GlobalEnv)
}

funs <- list("rowSums", "colSums", "sum", "exp", "log",
  "array", "cumsum")

# the following already seem to be generic: "rep", "as.vector", "length"

lapply(funs, convertToGeneric)
