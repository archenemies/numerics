# FHE 06 Apr 2025
# create generic methods

## rowSums.default <- function(...) { base::rowSums(...) }
## rowSums <- function(...) { UseMethod("rowSums") }

## colSums.default <- function(...) { base::colSums(...) }
## colSums <- function(...) { UseMethod("colSums") }

## sum.default <- function(...) { base::sum(...) }
## sum <- function(...) { UseMethod("sum") }

## exp.default <- function(...) { base::exp(...) }
## exp <- function(...) { UseMethod("exp") }

## log.default <- function(...) { base::log(...) }
## log <- function(...) { UseMethod("log") }

convertToGeneric <- function(fname) {
  base_fn = getFromNamespace(fname, "base")
#  base_fn = get(paste0("base::", fname))
  assign(paste0(fname, ".default"),
         base_fn,
         envir = .GlobalEnv)
  assign(fname,
         function(...) UseMethod(fname),
         envir = .GlobalEnv)
}

funs <- list("rowSums", "colSums", "sum", "exp", "log",
  "array")

# these already seem to be generic: "rep", "as.vector", "length"

lapply(funs, convertToGeneric)
