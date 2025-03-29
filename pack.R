# FHE 28 Mar 2025
# packing and unpacking parameter objects
# (moving between vectors and list of vectors)
# (from handwritten notes)

# packing and unpacking parameter objects
# - need to override "[" in basic_ops
# - pack_vec(named list or vector)
#   -> vector with names attribute
#   - named list -> all names start with "$"
#   - vector -> all names start with "[n]"
# - unpack_vec(named vector, template list, previx="")
#   -> named list of vectors
#   - opposite of pack_vec
#   - use template to guide unpacking, same as arg to pack_vec
# - both functions iterate through input list (or template), tracking indices in destination or source vector
#   - no need to get length in advance
#   - one function with if() stmt to pack/unpack
#   - recurse with destination vector and offset
#   - pack_or_unpack_vec(pack=T, list, vec_ref, vec_inds, prefix, index_env)
#     - both returns list
#     - pack updates vec_ref[vec_inds], unpack puts them in list
#     - to operate on sublist, use list$a and subset vec_inds
#     - index_env accumulates an index listing the vector indices of each list node
#       - environment of integer indices: a=1:5, a$b=1:2, a$c=3:5, etc.)
#     - prefix is for recursion, for generation of names() in pack() and in index_env keys
#     - don't bother checking vector names() in unpack, even though we put them there in pack(). this is too hard to do efficiently, and is why we use a template list for unpack
#     - pack_vec and unpack_vec just call this with pack=T or pack=F
#     - the index of nested subvectors that is output in index_env is also needed for possible subsetting of data frame columns, or Hessian submatrices. it can be an argument to pack_vec and unpack_vec
#
# - tool to aggregate (with sum or max) the vector entries under list paths like "du" command in unix.
#   - we can use this to find out which part of the parameters has the greatest change and to aggregate the changes in different parts of the parameter vector at each cycle
#
