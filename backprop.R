# FHE 26 Mar 2025
# backpropagation using tape-wrap.R

# from handwritten notes:
# - gradient calculation with backprop.
#   grad(x,y) - x and y are tape_wrap's
#     x could be a list of them
#     y must be wrapping a scalar value
#   1. compile a list of descendents of x (up to y)
#   2. initialize accumulators (one for each descendent)
#   3. work backwards from y, following inputs and accumulating adjoints in entries of list form 1 and 2
#   - this algorithm may not be optimal for all-paths (?) but its running time is dwarfed by the actual computation
#   - normally this would add entries to the tape, but we can have an option to suppress this (by unwrapping each input variable)
#   - every operation must have a back_ version, as in back_add, back_mat_mul, ... This takes as input the adjoint of the operator output, and returns a list of adjoints, one for each input (with NA for non-tape inputs)
#     back_add = function(adj_out, val, e1, e2) {
#         list(adj_out, adj_out) }
#     back_mul = function("""") {
#         list(adj_out*e2, adj_out*e1) }
#
# - to do hvp (hessian-vector product):
#   - we must run the computation over again with a dual number input
#     - we can re-use the tape but only if there is special recognition for dual_number datatype
#     - grad_dual(x,y) where x is a dual number (or list of them)
#       - we need to use the tape to do another forward pass to propagate the duals from x to y
#       - then y will be a dual number
#       - y-primal doesn't depend on x-dual
#       - assume we already did a backward pass to get dy/dx (primal), now we need to do another backward pass to get d(dy/dt)/dx
#         - we just set the y-dual-adjoint to 1 and the y-adjoint to 0 for this second pass
#         - this calculates the hessian vector product (the y-dual being the jacobian vector product) where the input vector is the x dual, dx/dt
#         - y = f(x(t)), dy/dt = f'(x(t))x'(t)
#           - d/dx(dy/dt) = f''(x) * x'
#           - is it written better in the paper?
#         - we are only interested in the backpropagated primal?

