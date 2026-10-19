# Splitting expressions into pipeline stages ------------------------------
#
# The native pipe `x |> f(y)` is rewritten by the parser into `f(x, y)`, so
# rewindr treats any chain of calls whose *first positional argument* is
# itself a call as a pipeline. magrittr pipes (`%>%`) are kept as-is and
# evaluated with magrittr semantics, so `.` placeholders keep working.

# Functions that are never treated as a pipeline stage.
.no_unwind <- c(
  "(", "{", "function", "<-", "<<-", "=", "if", "for", "while", "repeat",
  "~", "$", "@", "[[", "::", ":::", "+", "-", "*", "/", "^", "%%", "%/%",
  "&", "|", "!", "&&", "||", "==", "!=", "<", ">", "<=", ">=", "%in%",
  "c", "list", "quote", "bquote", "library", "require", "requireNamespace",
  "suppressWarnings", "suppressMessages", "invisible", "print", "return",
  "local", "with", "within", "tryCatch", "try", "system.time"
)

.magrittr_pipes <- c("%>%", "%T>%")

call_fun_name <- function(cl) {
  if (!is.call(cl)) return(NA_character_)
  head <- cl[[1]]
  if (is.symbol(head)) return(as.character(head))
  if (is.call(head) && is.symbol(head[[1]]) &&
      as.character(head[[1]]) %in% c("::", ":::")) {
    return(as.character(head[[3]]))
  }
  NA_character_
}

is_magrittr_pipe <- function(expr) {
  is.call(expr) && is.symbol(expr[[1]]) &&
    as.character(expr[[1]]) %in% .magrittr_pipes && length(expr) == 3
}

is_unwindable <- function(expr) {
  if (!is.call(expr) || length(expr) < 2) return(FALSE)
  fn <- call_fun_name(expr)
  if (is.na(fn) || fn %in% .no_unwind) return(FALSE)
  nm <- names(expr)
  first_is_positional <- is.null(nm) || !nzchar(nm[2])
  first_is_positional && (is.call(expr[[2]]) || is.symbol(expr[[2]]))
}

# Returns list(base = <expr>, stages = list(<stage>, ...)) with stages in
# execution order. A stage is list(mode = "first" | "magrittr", call, fun).
unwind_pipeline <- function(expr) {
  stages <- list()
  repeat {
    if (is_magrittr_pipe(expr)) {
      rhs <- expr[[3]]
      st <- list(mode = "magrittr", op = expr[[1]], rhs = rhs,
                 fun = if (is.call(rhs)) call_fun_name(rhs) else deparse1(rhs))
      stages <- c(list(st), stages)
      expr <- expr[[2]]
    } else if (is_unwindable(expr)) {
      st <- list(mode = "first", call = expr, fun = call_fun_name(expr))
      stages <- c(list(st), stages)
      if (is.symbol(expr[[2]])) {
        expr <- expr[[2]]
        break
      }
      expr <- expr[[2]]
    } else {
      break
    }
  }
  list(base = expr, stages = stages)
}

# Human-readable code for one stage, with the piped-in value shown as `.`.
stage_code <- function(stage) {
  if (stage$mode == "magrittr") return(deparse1(stage$rhs, collapse = " "))
  cl <- stage$call
  cl[[2]] <- as.name(".")
  deparse1(cl, collapse = " ")
}

# The call a stage would make if the input were bound to `.rw_in`, used both
# to evaluate it and to inspect its arguments (e.g. the `y` of a join).
stage_effective_call <- function(stage) {
  if (stage$mode == "first") {
    cl <- stage$call
    cl[[2]] <- as.name(".rw_in")
    return(cl)
  }
  rhs <- stage$rhs
  if (is.call(rhs) && !uses_dot(rhs)) {
    return(as.call(c(rhs[[1]], as.name(".rw_in"), as.list(rhs)[-1])))
  }
  NULL
}

# Elements of a call or expression, without empty arguments (as in `x[, 1]`).
call_parts <- function(e) {
  l <- as.list(e)
  l[!vapply(seq_along(l), function(i) identical(l[[i]], quote(expr = )), logical(1))]
}

uses_dot <- function(expr) {
  if (is.symbol(expr)) return(identical(expr, as.name(".")))
  if (is.call(expr)) return(any(vapply(call_parts(expr)[-1], uses_dot, logical(1))))
  FALSE
}

eval_stage <- function(stage, input, env) {
  e <- new.env(parent = env)
  assign(".rw_in", input, envir = e)
  if (stage$mode == "first") {
    return(eval(stage_effective_call(stage), e))
  }
  cl <- call("%>%", as.name(".rw_in"), stage$rhs)
  cl[[1]] <- stage$op
  eval(cl, e)
}
