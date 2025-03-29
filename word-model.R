# FHE 28 Mar 2025
# (toy) word model:
#
# (from handwritten notes)
# - lowercase words
# - no manip, but possible to play with this in future
# - model object has: (use environment type for model)
#   - params (dual_number)
#   - regularity
#   - gradient, hessian
#   - tape
#   - labeled_data
#   - word_vecs (+ frequency tag and whatever else)
# - data object has
#   - type: ("cmp", "class", "dict")
#   - args: (one or two word ids)
#   - label
#
# - functions to work with model:
#   - predict, loss
#   - total_loss (=-log likelihood), reg_total_loss, pert_ge
#   - emit_tape (calculates 3 above quantities and stores them as tape_wrap objects in model object. no need to use dual #s at this point)
#   - init_{nlcg,newton}, step_{"} (step_* calls emit_tape)
#   - some of these go in optim.R
#   - calc_rtl_gradient, calc_rtl_hessian, calc_ge_dual (for s optimization)
#   - print_model, vis_model
#     - print_model should show summary statistics
#       - change from previous model, number of labeled examples, training and generalization error, average loss, top and bottom predictions, parameter quartiles
#     - vis_model to "visualize" various parameters by looking at the top closest words
#     - in output, be as brief as possible but include the output of analysis functions that can be more verbose, e.g. pv(find_word(top=1,bot=1,model_hist[i]$params$word_query)), and then the user can re-run find_word with top=10 for more examples
# - functions to run AL loop:
#   - query_heuristic
#   - choose_next (-> data object, no label)
#   - solicit_label

# - use pack.R to pack/unpack parameters in model specification (mostly predict()?)

# - obviously, we need to support different models
