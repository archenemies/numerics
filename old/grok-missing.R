# original response
missing_to_NA <- function(...) {
  args <- substitute(list(...))[-1]
  result <- lapply(seq_along(args), function(i) {
    if (identical(args[[i]], quote(expr = ))) NA else eval(args[[i]])
  })
  as.list(result)
}

missing_to_NA <- function(...) {
  args <- substitute(list(...))[-1]
  result <- lapply(args, function(arg) {
    if (identical(arg, quote(expr = ))) NA else eval(arg)
  })
  as.list(result)
}

missing_to_NA <- function(...) {
  args <- substitute(list(...))[-1]
  result <- lapply(args, function(arg) {
    if (arg == quote(expr = )) NA else eval(arg)
  })
  as.list(result)
}

# my version before asking Grok
missing_to_NA = function(...) {
  lapply(as.list(match.call()[-1]),
    function(arg) {
      if(is.name(arg) && arg=="") {
        NA
      } else {
        pv(arg)
        eval(arg)
      }
    }
  )
}
