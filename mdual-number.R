# -*- my-source-delegate: "test-mdual-number.R" -*-
# FHE 24 Mar 2025
# Defines mdual_number class

# For arithmetic with propagation of multiple (independent) dual
# number components

# First version with a very little help from Grok
# https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa

mysource("lastind.R")

mdual_number <- function(value, mdual) {
  # Validate inputs
  stopifnot(is.numeric(value))
  stopifnot(is.numeric(mdual))
  stopifnot(is.character(names))

  ndim = length(dim(mdual))
  stopifnot(ndim>0)
  K = dim(mdual)[ndim]
  if(!identical(c(dim(value), K), dim(mdual))) {
    stop("non-conforming dimensions for value and mdual: ",
      sv(dim(value)), "; ", sv(dim(mdual)))
  }
  # Create and return the object
  structure(list_vars(value, mdual), class = "mdual_number")
}

is.mdual_number = function(x) { inherits(x, "mdual_number") }

print.mdual_number = function(x) {
  ns = x$names
  cat("mdual_number: ", paste0(ns, collapse=","), "\n")
  cat("  primal\n")
  print(x$value)
  cat("  mdual\n")
  print(x$mdual)
}

auto_vec_ops <-
  c("+", "*", "-", "/", "^",
  "exp","log",
  "sin","cos","tan")

