# Recording engine -------------------------------------------------------

#' Record every step of a pipeline or code block
#'
#' Evaluates `expr` one step at a time and keeps a snapshot of the data after
#' each step. A pipeline (`|>` or `%>%`) is split into its stages; a `{ }`
#' block is run statement by statement, like a script. Code runs in a
#' sandbox environment whose parent is `env`, so your workspace is not
#' modified. Errors do not abort recording: the failing step is kept so you
#' can rewind to the state just before it.
#'
#' @param expr A pipeline expression or a braced block of statements.
#' @param key Optional column(s) that uniquely identify a row; enables
#'   row-level lineage (which keys were dropped, which rows changed).
#' @param rules Expectations from [rewind_rules()].
#' @param env Environment the code is evaluated against.
#' @param label Name for the final result of a bare pipeline.
#' @return A `rewind_trace` object.
#' @examples
#' tr <- rewind(mtcars |> subset(cyl == 4) |> transform(kpl = mpg * 0.425))
#' tr
#' at_step(tr, 2)
#' @export
rewind <- function(expr, key = NULL, rules = rewind_rules(), env = parent.frame(),
                   label = "result") {
  expr <- substitute(expr)
  if (is.call(expr) && identical(expr[[1]], as.name("{"))) {
    exprs <- as.list(expr)[-1]
    srcrefs <- attr(expr, "srcref")
    srcrefs <- if (length(srcrefs) == length(expr)) srcrefs[-1] else NULL
  } else {
    exprs <- list(expr)
    srcrefs <- NULL
  }
  run_trace(exprs, srcrefs, env = env, key = key, rules = rules, label = label,
            source = paste(vapply(exprs, deparse1, character(1), collapse = "\n"), collapse = "\n"))
}

#' Record every step of an R script
#'
#' Runs a script in a sandbox, statement by statement, splitting every
#' pipeline into stages and recording each version of every data frame.
#'
#' @param file Path to an `.R` file.
#' @param text Alternatively, the code as a character string.
#' @param chdir Temporarily change the working directory to the script's
#'   folder (like `source(chdir = TRUE)`).
#' @inheritParams rewind
#' @return A `rewind_trace` object.
#' @examples
#' tr <- rewind_script(text = c(
#'   "raw <- data.frame(id = 1:5, v = c(4, NA, 9, 1, 7))",
#'   "kept <- raw |> subset(v > 2)"
#' ))
#' tr
#'
#' # The bundled demo pipeline (needs dplyr)
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   tr <- rewind_script(rewind_example(), key = "order_id")
#'   issues(tr)
#' }
#' @export
rewind_script <- function(file = NULL, text = NULL, key = NULL, rules = rewind_rules(),
                          env = globalenv(), chdir = TRUE) {
  if (is.null(text)) {
    stopifnot(file.exists(file))
    text <- readLines(file, warn = FALSE)
    if (chdir) {
      old <- setwd(dirname(normalizePath(file)))
      on.exit(setwd(old), add = TRUE)
    }
  }
  text <- paste(text, collapse = "\n")
  exprs <- parse(text = text, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")
  tr <- run_trace(as.list(exprs), srcrefs, env = env, key = key, rules = rules,
                  label = "result", source = text)
  tr$file <- file
  tr
}

snapshot_value <- function(x) {
  if (inherits(x, "data.table") && requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::copy(x))
  }
  x
}

assignment_parts <- function(expr) {
  if (is.call(expr) && is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("<-", "=") &&
      length(expr) == 3 && is.symbol(expr[[2]])) {
    return(list(target = as.character(expr[[2]]), rhs = expr[[3]]))
  }
  NULL
}

# Flattens multi-line (rlang/cli style) condition messages into one line.
clean_condition <- function(msg) {
  lines <- trimws(strsplit(msg, "\n", fixed = TRUE)[[1]])
  lines <- gsub("^[!\u2139\u2715\u00d7x*i] ", "", lines)
  lines <- lines[nzchar(lines)]
  lines <- lines[!grepl("^Caused by (warning|error)", lines)]
  paste(lines, collapse = " ")
}

# Evaluates `fn()` capturing warnings (muffled) and errors.
capture_eval <- function(fn) {
  warnings <- character(0)
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(
    withCallingHandlers(list(value = fn(), error = NULL),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")),
    error = function(e) list(value = NULL, error = clean_condition(conditionMessage(e)))
  )
  res$warnings <- unique(vapply(warnings, clean_condition, character(1), USE.NAMES = FALSE))
  res$elapsed <- proc.time()[["elapsed"]] - t0
  res
}

run_trace <- function(exprs, srcrefs, env, key, rules, label, source) {
  sandbox <- new.env(parent = env)
  steps <- list()
  versions <- list()          # object name -> id of the step holding its latest version
  status <- "ok"

  add_step <- function(...) {
    s <- list(...)
    s$id <- length(steps) + 1L
    steps[[s$id]] <<- s
    s$id
  }
  latest_value <- function(name) {
    id <- versions[[name]]
    if (is.null(id)) NULL else steps[[id]]$value
  }

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    sr <- if (!is.null(srcrefs)) srcrefs[[i]] else NULL
    code <- if (!is.null(sr)) paste(as.character(sr), collapse = "\n") else deparse1(expr, collapse = "\n")
    line <- if (!is.null(sr)) sr[[1]] else NA_integer_
    asg <- assignment_parts(expr)
    target <- if (!is.null(asg)) asg$target else if (length(exprs) == 1) label else NA_character_
    rhs <- if (!is.null(asg)) asg$rhs else expr
    # Only split values that are kept (assigned, or the single expression
    # given to rewind()); bare statements such as `dt[, b := 1]` or `print(x)`
    # run as-is so in-place modifications are recorded as new versions.
    pipe <- if (!is.null(asg) || length(exprs) == 1) unwind_pipeline(rhs) else list(stages = list())

    if (length(pipe$stages) > 0) {
      # Pipeline: record the input, then each stage.
      base <- pipe$base
      input_id <- NA_integer_
      if (is.symbol(base) && !is.null(versions[[as.character(base)]])) {
        input_id <- versions[[as.character(base)]]
        current <- snapshot_value(steps[[input_id]]$value)
      } else {
        r <- capture_eval(function() eval(base, sandbox))
        kind <- if (is.symbol(base)) "source" else "step"
        input_id <- add_step(stmt = i, line = line, code = deparse1(base, collapse = " "),
                             stmt_code = code, fun = call_fun_name(base), object = if (is.symbol(base)) as.character(base) else NA_character_,
                             chain = target, input = NA_integer_, kind = kind,
                             value = snapshot_value(r$value), error = r$error,
                             warnings = r$warnings, elapsed = r$elapsed, join = NULL)
        if (is.symbol(base) && is.null(r$error)) versions[[as.character(base)]] <- input_id
        if (!is.null(r$error)) { status <- "error"; break }
        current <- r$value
      }
      failed <- FALSE
      for (k in seq_along(pipe$stages)) {
        st <- pipe$stages[[k]]
        last <- k == length(pipe$stages)
        r <- capture_eval(function() eval_stage(st, current, sandbox))
        jr <- if (is.null(r$error) && is_join_fun(st$fun))
          tryCatch(analyse_join(st, current, sandbox), error = function(e) NULL) else NULL
        input_id <- add_step(stmt = i, line = line, code = stage_code(st), stmt_code = code,
                             fun = st$fun, object = if (last) target else NA_character_,
                             chain = target, input = input_id, kind = "step",
                             value = snapshot_value(r$value), error = r$error,
                             warnings = r$warnings, elapsed = r$elapsed, join = jr,
                             vars = all.vars(if (st$mode == "first") st$call else st$rhs))
        if (!is.null(r$error)) { failed <- TRUE; break }
        current <- r$value
      }
      if (failed) { status <- "error"; break }
      if (!is.na(target)) {
        if (!is.null(asg)) assign(target, current, envir = sandbox)
        versions[[target]] <- input_id
      }
    } else {
      # Plain statement: run it and record every object it created or changed.
      before <- ls(sandbox)
      r <- capture_eval(function() eval(expr, sandbox))
      if (!is.null(r$error)) {
        add_step(stmt = i, line = line, code = code, stmt_code = code, fun = call_fun_name(rhs),
                 object = target, chain = target, input = if (!is.na(target) && !is.null(versions[[target]])) versions[[target]] else NA_integer_,
                 kind = "step", value = NULL, error = r$error, warnings = r$warnings,
                 elapsed = r$elapsed, join = NULL)
        status <- "error"
        break
      }
      # Record data frames that are new or differ from their last recorded
      # version, plus the statement's assignment target whatever its type.
      changed <- character(0)
      for (nm in ls(sandbox)) {
        val <- get(nm, envir = sandbox)
        if (is.function(val)) next
        if (!is_df(val) && !identical(nm, target)) next
        if (!is.null(versions[[nm]])) {
          if (!identical(latest_value(nm), val)) changed <- c(changed, nm)
        } else if (!(nm %in% before) || identical(nm, target) || is_df(val)) {
          changed <- c(changed, nm)
        }
      }
      for (nm in changed) {
        val <- get(nm, envir = sandbox)
        add_step(stmt = i, line = line, code = code, stmt_code = code, fun = call_fun_name(rhs),
                 object = nm, chain = nm,
                 input = if (!is.null(versions[[nm]])) versions[[nm]] else NA_integer_,
                 kind = "step", value = snapshot_value(val), error = NULL,
                 warnings = r$warnings, elapsed = r$elapsed, join = NULL)
        versions[[nm]] <- length(steps)
      }
      if (length(exprs) == 1 && !length(changed)) {
        add_step(stmt = i, line = line, code = code, stmt_code = code, fun = call_fun_name(rhs),
                 object = label, chain = label, input = NA_integer_, kind = "step",
                 value = snapshot_value(r$value), error = NULL, warnings = r$warnings,
                 elapsed = r$elapsed, join = NULL)
        versions[[label]] <- length(steps)
      }
    }
  }

  tr <- structure(list(steps = steps, versions = versions, key = key, rules = rules,
                       env = sandbox, parent_env = env, source = source, status = status,
                       created = Sys.time(), file = NULL),
                  class = "rewind_trace")
  analyse_trace(tr)
}

# Adds summaries, diffs and issues to every step.
analyse_trace <- function(tr) {
  for (i in seq_along(tr$steps)) {
    s <- tr$steps[[i]]
    s$summary <- if (is.null(s$error)) summarise_value(s$value) else NULL
    prev <- if (!is.na(s$input)) tr$steps[[s$input]] else NULL
    s$diff <- if (!is.null(prev) && is_df(prev$value) && is_df(s$value))
      tryCatch(df_diff(prev$value, s$value, key = tr$key), error = function(e) NULL) else NULL
    s$issues <- check_step(s, prev, tr$rules, tr$key)
    tr$steps[[i]] <- s
  }
  tr
}

# Accessors ----------------------------------------------------------------

check_step_id <- function(tr, i) {
  if (is.character(i)) {
    id <- tr$versions[[i]]
    if (is.null(id)) stop("No recorded object named '", i, "'.", call. = FALSE)
    return(id)
  }
  if (i < 1 || i > length(tr$steps)) stop("Step must be between 1 and ", length(tr$steps), call. = FALSE)
  as.integer(i)
}

#' Get the value of the data at a recorded step
#'
#' @param tr A `rewind_trace`.
#' @param i Step number, or an object name for its final version.
#' @return The recorded value (usually a data frame).
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' at_step(tr, "clean")
#' at_step(tr, 2)
#' @export
at_step <- function(tr, i) tr$steps[[check_step_id(tr, i)]]$value

#' What changed at a step
#'
#' @inheritParams at_step
#' @param against Step to compare with; defaults to the step's input.
#' @return A `rewind_diff`, or `NULL` when either side is not a data frame.
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' step_diff(tr, "clean")
#' step_diff(tr, "clean", against = "orders")
#' @export
step_diff <- function(tr, i, against = NULL) {
  i <- check_step_id(tr, i)
  if (is.null(against)) return(tr$steps[[i]]$diff)
  df_diff(at_step(tr, against), at_step(tr, i), key = tr$key)
}

#' Timeline of recorded steps
#'
#' @inheritParams at_step
#' @return A data frame with one row per step: its number, source line,
#'   object, code, rows, columns, change in rows, status and run time.
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' timeline(tr)
#' @export
timeline <- function(tr) {
  if (!length(tr$steps)) return(data.frame())
  rows <- lapply(tr$steps, function(s) {
    sm <- s$summary
    isdf <- !is.null(sm) && sm$type == "data.frame"
    sev <- if (is.null(s$issues)) "" else worst_severity(s$issues$severity)
    data.frame(
      step = s$id, line = s$line, object = if (is.na(s$object)) "" else s$object,
      chain = if (is.na(s$chain)) "" else s$chain,
      code = s$code, label = step_label(s), rows = if (isdf) sm$nrow else NA_integer_,
      cols = if (isdf) sm$ncol else NA_integer_,
      row_delta = if (!is.null(s$diff)) s$diff$row_delta else NA_integer_,
      class = if (!is.null(sm)) sm$class else "error",
      status = if (!is.null(s$error)) "error" else if (nzchar(sev)) sev else "ok",
      n_issues = if (is.null(s$issues)) 0L else nrow(s$issues),
      ms = round(1000 * s$elapsed),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# One-line description of a step: "obj <- code" for the step that produced
# an object, the bare stage code otherwise.
step_label <- function(s) {
  code <- strsplit(s$code, "\n", fixed = TRUE)[[1]]
  first <- if (length(code) > 1) paste0(code[1], " ...") else code
  if (identical(s$kind, "source") || is.na(s$object) || startsWith(trimws(s$code), s$object)) return(first)
  paste0(s$object, " <- ", first)
}

worst_severity <- function(sev) {
  for (lv in c("error", "warning", "info")) if (lv %in% sev) return(lv)
  ""
}

#' All issues found while recording
#'
#' @inheritParams at_step
#' @param min_severity Lowest severity to include: "info", "warning" or "error".
#' @return A data frame with one row per issue: step, line, object, code,
#'   severity, type, message and a suggested fix (`hint`).
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' issues(tr)
#' issues(tr, "info")
#' @export
issues <- function(tr, min_severity = "warning") {
  lv <- c(info = 1, warning = 2, error = 3)
  rows <- lapply(tr$steps, function(s) {
    if (is.null(s$issues)) return(NULL)
    cbind(step = s$id, line = s$line, object = if (is.na(s$chain)) "" else s$chain,
          code = s$code, s$issues, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    return(data.frame(step = integer(), line = integer(), object = character(), code = character(),
                      severity = character(), type = character(), message = character(),
                      hint = character(), stringsAsFactors = FALSE))
  }
  out <- out[lv[out$severity] >= lv[[min_severity]], , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The first step where the data diverged from expectations
#'
#' @inheritParams at_step
#' @return The step id, or `NA` when nothing suspicious was found.
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' first_divergence(tr)
#' @export
first_divergence <- function(tr) {
  iss <- issues(tr, "warning")
  if (!nrow(iss)) NA_integer_ else min(iss$step)
}

#' Lineage of an object
#'
#' Follows the recorded inputs backwards from the final version of `object`
#' and returns the step ids, oldest first.
#'
#' @inheritParams at_step
#' @param object Object name (or a step id to start from).
#' @return Integer vector of step ids.
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' lineage(tr, "clean")
#' @export
lineage <- function(tr, object) {
  id <- check_step_id(tr, object)
  ids <- integer(0)
  while (!is.na(id)) {
    ids <- c(id, ids)
    id <- tr$steps[[id]]$input
  }
  ids
}

#' Find where an object went wrong
#'
#' Walks the lineage of `object` and reports the earliest step that explains
#' its current state. With `column`, it finds where that column was created,
#' changed type, or gained missing values; otherwise it reports the first
#' suspicious step in the lineage.
#'
#' @inheritParams lineage
#' @param column Optional column name.
#' @return An object of class `rewind_blame`; print it for a readable report.
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' blame(tr, "clean")
#' blame(tr, "clean", "amount")
#' @export
blame <- function(tr, object, column = NULL) {
  ids <- lineage(tr, object)
  findings <- list()
  for (id in ids) {
    s <- tr$steps[[id]]
    if (is.null(column)) {
      if (!is.null(s$issues) && any(s$issues$severity %in% c("warning", "error"))) {
        iss <- s$issues[s$issues$severity %in% c("warning", "error"), , drop = FALSE]
        findings[[length(findings) + 1]] <- list(step = id, what = iss$message, hint = iss$hint)
      }
    } else {
      d <- s$diff
      msgs <- character(0)
      if (is.null(d) && is_df(s$value) && column %in% names(s$value) && id == ids[1]) {
        msgs <- c(msgs, sprintf("'%s' present in the source data (%s NA).", column, fmt_n(sum(is.na(s$value[[column]])))))
      }
      if (!is.null(d)) {
        if (column %in% d$cols_added) msgs <- c(msgs, sprintf("'%s' was created here.", column))
        if (column %in% d$type_changes$column) {
          tc <- d$type_changes[d$type_changes$column == column, ]
          msgs <- c(msgs, sprintf("'%s' changed type %s -> %s.", column, tc$from, tc$to))
        }
        if (column %in% d$na_delta$column) {
          nd <- d$na_delta[d$na_delta$column == column, ]
          msgs <- c(msgs, sprintf("'%s' gained %s NA values.", column, fmt_n(nd$new_na)))
        }
        if (column %in% names(d$changed_cells)) {
          msgs <- c(msgs, sprintf("%s values of '%s' changed.", fmt_n(d$changed_cells[[column]]), column))
        }
        if (column %in% d$cols_removed) msgs <- c(msgs, sprintf("'%s' was removed here.", column))
      }
      if (length(msgs)) findings[[length(findings) + 1]] <- list(step = id, what = msgs, hint = NA_character_)
    }
  }
  structure(list(object = object, column = column, lineage = ids, findings = findings, trace = tr),
            class = "rewind_blame")
}

#' @export
print.rewind_blame <- function(x, ...) {
  tgt <- if (is.null(x$column)) x$object else paste0(x$object, "$", x$column)
  cat(sprintf("Lineage of %s: %d steps\n", tgt, length(x$lineage)))
  if (!length(x$findings)) {
    cat("Nothing suspicious found along this lineage.\n")
    return(invisible(x))
  }
  for (f in x$findings) {
    s <- x$trace$steps[[f$step]]
    loc <- if (!is.na(s$line)) sprintf(" (line %d)", s$line) else ""
    cat(sprintf("\n> step %d%s: %s\n", f$step, loc, step_label(s)))
    for (k in seq_along(f$what)) {
      cat("    ", f$what[k], "\n", sep = "")
      if (!is.na(f$hint[k])) cat("    fix: ", f$hint[k], "\n", sep = "")
    }
  }
  invisible(x)
}

#' @export
print.rewind_trace <- function(x, ...) {
  tl <- timeline(x)
  cat(sprintf("<rewind_trace> %d steps recorded%s\n", nrow(tl),
              if (x$status == "error") " (stopped at an error)" else ""))
  if (!nrow(tl)) return(invisible(x))
  icon <- c(ok = "  ", info = " i", warning = " !", error = " x")
  code <- ifelse(nchar(tl$label) > 58, paste0(substr(tl$label, 1, 55), "..."), tl$label)
  shape <- ifelse(is.na(tl$rows), tl$class, sprintf("%s x %s", fmt_n(tl$rows), tl$cols))
  delta <- ifelse(is.na(tl$row_delta) | tl$row_delta == 0, "", sprintf("%+d", tl$row_delta))
  for (i in seq_len(nrow(tl))) {
    cat(sprintf("%s %3d  %-58s %14s %8s\n", icon[[tl$status[i]]], tl$step[i],
                code[i], shape[i], delta[i]))
  }
  fd <- first_divergence(x)
  if (!is.na(fd)) {
    iss <- issues(x, "warning")
    iss <- iss[iss$step == fd, ]
    cat(sprintf("\nFirst divergence at step %d: %s\n", fd, iss$message[1]))
    if (!is.na(iss$hint[1])) cat("  fix: ", iss$hint[1], "\n", sep = "")
    cat("Run scrub(tr) to explore, or issues(tr) for the full list.\n")
  } else {
    cat("\nNo suspicious steps found.\n")
  }
  invisible(x)
}

#' @export
summary.rewind_trace <- function(object, ...) timeline(object)
