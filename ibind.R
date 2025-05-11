# FHE 11 May 2025
# from initial ChatGPT prompt

# helper function
`%upto%` = function (from, to)
  if (from <= to) from:to else numeric(0)

# ibind "index bind"
# Combine several indexed named objects into an indexed named object
# Requires inputs to be indexed
# u = ibind(dim=1, a=v, b=abs(v))
# - the argument objects should have all dimensions equal except for
# the binding dimension (and dimnames equal, if set)
# - sets "dimindices" attribute in addition to "dimnames"
# - dimindices(u) is a list of lists, analogous to dimnames
#   - the outer list is the same length as dim(u)
#   - each inner list, if not NULL, maps character index keys to integer index vectors
# - ibind uses (and requires) "dimindices" attributes in arguments, where names exist
# - for example if v has an element name "x" then it will be named "a$x" in the output, dimindices(u)[[1]][["a"]] will be 1 %upto% length(v), dimindices(u)[[1]][["a$x"]] will be the same as dimindices(v)[[1]][["x"]], a single integer indexing the "x" element of v; dimindices(u)[[1]][["b"]] will be (length(v)+1) %upto% (length(v)+length(v)), and so on.
# - if v is not named, then u will have names like "a$1", "a$2", "b$1", etc. These numerical names don't have to be in the index for u, since the user could just call ifetch(u, "a")[[2]] instead of ifetch(u, "a$2").
# - we require all ... arguments to be named
ibind = function(dim=NULL, ...) {
  # XXX
}

# iprep "prepare index"
# Sets "dimindices" attribute from dimnames()
iprep = function(obj) {
  # XXX
}

# ifetch(u, "a", dim=1)
# ifetch(u, "b$x")
# Subscript the given object with the named indices along the specified dimension.
# Error if named indices don't exist.
ifetch = function(obj, ixkey, dim=NULL) {
  # XXX
}
