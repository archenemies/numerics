# FHE 06 Apr 2025
# create generic methods

rowSums.default <- function(...) { base::rowSums(...) }
rowSums <- function(...) { UseMethod("rowSums") }

colSums.default <- function(...) { base::colSums(...) }
colSums <- function(...) { UseMethod("colSums") }

sum.default <- function(...) { base::sum(...) }
sum <- function(...) { UseMethod("sum") }

exp.default <- function(...) { base::exp(...) }
exp <- function(...) { UseMethod("exp") }

log.default <- function(...) { base::log(...) }
log <- function(...) { UseMethod("log") }
