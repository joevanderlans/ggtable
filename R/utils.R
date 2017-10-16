# grab all arguments to a function
find_args <- function() {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))

  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]

  vals
}

is_missing_arg <- function(x) identical(x, quote(expr = ))
