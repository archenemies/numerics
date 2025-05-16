source("bbind.R")

cat("Running bbound unit tests...\n")

# Test 1: bprep and dimblocks structure
v1 = array(1:3, dim = c(3,1))
dimnames(v1) = list(c("x", "y", "z"), NULL)
v1 = bprep(v1)

stopifnot(inherits(v1, "bbound"))
stopifnot(!is.null(dimblocks(v1)))
stopifnot(length(dimblocks(v1)[[1]]) == 3)
stopifnot(identical(dimblocks(v1)[[1]]$x$ixs, 1))

# Test 2: bbind basic two-block bind
v2 = array(4:6, dim = c(3,1))
dimnames(v2) = list(c("u", "v", "w"), NULL)
v2 = bprep(v2)

u = bbind(dim = 1, a = v1, b = v2)

stopifnot(dim(u)[1] == 6)
stopifnot(identical(u[1,], 1))
stopifnot(identical(u[6,], 6))
stopifnot(identical(dimnames(u)[[1]], c("a$x", "a$y", "a$z", "b$u", "b$v", "b$w")))

# Test 3: bfetch single-level block
blk_a = bfetch(u, "a", dim = 1)
stopifnot(identical(blk_a$ixs, 1:3))

# Test 4: bfetch nested subblock
blk_av = bfetch(u, "b$v", dim = 1)
stopifnot(identical(blk_av$ixs, 5))

# Test 5: subscript access via block path
stopifnot(identical(u["a", , drop = TRUE], v1))
stopifnot(identical(u["b$w", ], 6))

# Test 6: invalid block access errors
error_raised = FALSE
tryCatch({
  bfetch(u, "c", dim = 1)
}, error = function(e) {
  error_raised <<- TRUE
})
stopifnot(error_raised)

cat("All tests passed!\n")
