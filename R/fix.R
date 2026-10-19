# Fix suggestions ----------------------------------------------------------

#' Rule-based fix suggestions
#'
#' Turns the issues found in a trace into concrete code suggestions. Works
#' offline; see [ask_claude()] for full rewrites from a natural-language
#' instruction.
#'
#' @inheritParams at_step
#' @return A data frame with `step`, `line`, `problem` and `suggestion`.
#' @examples
#' tr <- rewind({
#'   orders <- data.frame(id = 1:6, cust = c("A", "B", " b", "C", "D", "A"),
#'                        amount = c("10", "25", "$7", "40", "12", "8"))
#'   customers <- data.frame(cust = c("A", "B", "C", "C"), region = c("N", "S", "E", "W"))
#'   clean <- orders |>
#'     transform(amount = as.numeric(amount)) |>
#'     merge(customers, by = "cust")
#' })
#' suggest_fixes(tr)
#' @export
suggest_fixes <- function(tr) {
  out <- list()
  add <- function(s, problem, suggestion) {
    out[[length(out) + 1]] <<- data.frame(step = s$id, line = s$line, problem = problem,
                                          suggestion = suggestion, stringsAsFactors = FALSE)
  }
  for (s in tr$steps) {
    if (is.null(s$issues)) next
    types <- s$issues$type
    j <- s$join
    if (!is.null(j) && "join_unmatched" %in% types && j$n_near_miss > 0) {
      k <- j$by_x[1]; ky <- j$by_y[1]
      add(s, "Join keys differ only by whitespace/case.",
          sprintf("Normalise keys on both sides before the join:\n  mutate(%s = toupper(trimws(%s)))   # on the left table\n  mutate(%s = toupper(trimws(%s)))   # on the lookup table", k, k, ky, ky))
    } else if (!is.null(j) && "join_unmatched" %in% types) {
      add(s, "Rows found no match in the join.",
          sprintf("List them first: anti_join(<left>, <lookup>, by = %s)", deparse1(j$by_x)))
    }
    if ("join_fanout" %in% types) {
      key <- if (!is.null(j)) j$by_y else "<key>"
      add(s, "Lookup table has duplicated keys, so rows were multiplied.",
          sprintf("De-duplicate the lookup table first: distinct(<lookup>, %s, .keep_all = TRUE)", paste(key, collapse = ", ")))
    }
    if ("join_key_type" %in% types) {
      add(s, "Join keys have different types.", "Cast both keys to character (or both to integer) before joining.")
    }
    coerced <- intersect(s$diff$na_delta$column, s$diff$type_changes$column)
    if (length(coerced) || any(grepl("coercion", s$issues$message))) {
      col <- if (length(coerced)) coerced[1] else "<column>"
      bad <- examples_unparsed(tr, s, col)
      add(s, sprintf("'%s' has values that do not parse as numbers%s.", col,
                     if (length(bad)) paste0(" (e.g. ", paste(shQuote(bad), collapse = ", "), ")") else ""),
          sprintf("Strip non-numeric characters before converting:\n  %s = as.numeric(gsub(\"[^0-9.eE+-]\", \"\", %s))\n  # or readr::parse_number(%s)", col, col, col))
    }
    if ("filter_drops_na" %in% types) {
      add(s, "The filter silently drops rows where its columns are NA.",
          "Fix the NA values upstream (see the step that introduced them), or keep them explicitly with `| is.na(col)`.")
    }
    if ("empty" %in% types) {
      add(s, "The step returned zero rows.", "Inspect the previous step's data: at_step(tr, <previous step>).")
    }
  }
  if (!length(out)) return(data.frame(step = integer(), line = integer(), problem = character(), suggestion = character()))
  do.call(rbind, out)
}

# Values of `col` in the step's input that became NA in its output.
examples_unparsed <- function(tr, s, col, n = 3) {
  if (is.na(s$input) || !is_df(s$value)) return(character(0))
  before <- tr$steps[[s$input]]$value
  if (!is_df(before) || !col %in% names(before) || nrow(before) != nrow(s$value)) return(character(0))
  idx <- which(is.na(s$value[[col]]) & !is.na(before[[col]]))
  utils::head(unique(as.character(before[[col]][idx])), n)
}

# Claude -----------------------------------------------------------------

# Builds a compact, text-only description of the trace for the model.
trace_context <- function(tr, step = NULL, max_rows = 5) {
  tl <- timeline(tr)
  iss <- issues(tr, "info")
  lines <- c("## Source code", "```r", tr$source, "```", "",
             "## Recorded timeline (one row per step)",
             paste(utils::capture.output(print(tl[, c("step", "line", "label", "rows", "cols", "row_delta", "status")], row.names = FALSE)), collapse = "\n"),
             "", "## Issues detected")
  if (nrow(iss)) {
    for (i in seq_len(nrow(iss))) {
      lines <- c(lines, sprintf("- step %d (line %s) [%s/%s] %s%s", iss$step[i], iss$line[i], iss$severity[i], iss$type[i],
                                iss$message[i], if (!is.na(iss$hint[i])) paste0(" Hint: ", iss$hint[i]) else ""))
    }
  } else lines <- c(lines, "- none")
  focus <- unique(c(step, iss$step[iss$severity != "info"]))
  focus <- focus[!is.na(focus)]
  for (id in utils::head(focus, 6)) {
    s <- tr$steps[[id]]
    lines <- c(lines, "", sprintf("## Step %d: `%s`", id, s$code))
    if (!is.na(s$input) && is_df(tr$steps[[s$input]]$value)) {
      lines <- c(lines, "Input sample:", "```", utils::capture.output(print(utils::head(tr$steps[[s$input]]$value, max_rows))), "```")
    }
    if (is_df(s$value)) {
      lines <- c(lines, "Output sample:", "```", utils::capture.output(print(utils::head(s$value, max_rows))), "```")
    }
    if (!is.null(s$join)) {
      j <- s$join
      lines <- c(lines, sprintf("Join report: %s unmatched rows; unmatched key examples: %s; near-miss keys: %s; duplicated lookup keys: %s",
                                j$unmatched_rows, paste(shQuote(j$unmatched_examples), collapse = ", "),
                                paste(shQuote(j$near_miss_keys), collapse = ", "), paste(shQuote(j$dup_y_examples), collapse = ", ")))
    }
  }
  paste(lines, collapse = "\n")
}

.claude_schema <- list(
  type = "object",
  properties = list(
    diagnosis = list(type = "string", description = "Root cause of the problems, referring to step numbers."),
    changes = list(type = "array", items = list(type = "string"), description = "One short sentence per change made."),
    code = list(type = "string", description = "The complete revised R script.")
  ),
  required = list("diagnosis", "changes", "code"),
  additionalProperties = FALSE
)

.claude_system <- paste(
  "You are a data engineering assistant embedded in rewindr, an R time-travel debugger.",
  "You receive an R script, a step-by-step record of how each data frame changed while it ran,",
  "and the anomalies rewindr detected. Return a corrected version of the whole script.",
  "Keep the author's style and libraries, change only what is needed, and keep the same object names",
  "so the new run can be compared with the old one. Prefer fixes at the root cause (the earliest step",
  "that explains a problem) over patches downstream. If the user gives an instruction, apply it as well."
)

#' Ask Claude to diagnose and rewrite the recorded code
#'
#' Sends the code, the recorded timeline and the detected issues to the
#' Claude API and returns a diagnosis plus a complete revised script. Nothing
#' is executed: run the result with [rewind_script()] (`text = fix$code`) and
#' compare with [compare_traces()].
#'
#' The data samples included are the first few rows of the steps with issues;
#' set `max_rows = 0` to send no data values.
#'
#' @inheritParams at_step
#' @param instruction Optional natural-language request, e.g. "make the
#'   monthly summary resilient to missing months and fill them with zero".
#' @param step Optional step id to focus on.
#' @param model Claude model id.
#' @param effort Reasoning effort: "low", "medium", "high", "xhigh" or "max".
#' @param max_rows Rows of sample data to include per step.
#' @param api_key Anthropic API key.
#' @return A list of class `rewind_fix` with `diagnosis`, `changes` and `code`.
#' @examples
#' \dontrun{
#' # Needs an Anthropic API key in ANTHROPIC_API_KEY
#' tr <- rewind_script(rewind_example())
#' fix <- ask_claude(tr, "Also fill months with no orders with zero revenue")
#' fix
#' new <- rewind_script(text = fix$code)
#' compare_traces(tr, new)
#' }
#' @export
ask_claude <- function(tr, instruction = NULL, step = NULL, model = "claude-opus-5-5",
                       effort = "high", max_rows = 5,
                       api_key = Sys.getenv("ANTHROPIC_API_KEY")) {
  if (!requireNamespace("httr2", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("ask_claude() needs the httr2 and jsonlite packages.", call. = FALSE)
  }
  if (!nzchar(api_key)) {
    stop("Set the ANTHROPIC_API_KEY environment variable (or pass api_key).", call. = FALSE)
  }
  prompt <- paste0(trace_context(tr, step, max_rows),
                   "\n\n## Request\n",
                   if (is.null(instruction) || !nzchar(instruction))
                     "Fix the problems rewindr detected." else instruction)
  body <- list(
    model = model,
    max_tokens = 16000,
    system = .claude_system,
    thinking = list(type = "adaptive"),
    output_config = list(effort = effort, format = list(type = "json_schema", schema = .claude_schema)),
    fallbacks = "default",
    messages = list(list(role = "user", content = prompt))
  )
  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(`x-api-key` = api_key, `anthropic-version` = "2023-06-01",
                       `anthropic-beta` = "server-side-fallback-2026-07-01",
                       `content-type` = "application/json") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(600) |>
    httr2::req_retry(max_tries = 3, is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 529)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (httr2::resp_status(resp) >= 400) {
    stop("Claude API error (", httr2::resp_status(resp), "): ", out$error$message %||% "unknown", call. = FALSE)
  }
  if (identical(out$stop_reason, "refusal")) {
    stop("Claude declined this request.", call. = FALSE)
  }
  if (identical(out$stop_reason, "max_tokens")) {
    stop("The response was cut off (max_tokens). Try a shorter script or a focused `step`.", call. = FALSE)
  }
  txt <- NULL
  for (b in out$content) if (identical(b$type, "text")) txt <- paste0(txt, b$text)
  parsed <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
  structure(list(diagnosis = parsed$diagnosis, changes = parsed$changes, code = parsed$code,
                 model = out$model, usage = out$usage),
            class = "rewind_fix")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
print.rewind_fix <- function(x, ...) {
  cat("Diagnosis:\n", strwrap(x$diagnosis, indent = 2, exdent = 2), sep = "\n")
  cat("\nChanges:\n", paste0("  - ", x$changes), sep = "\n")
  cat("\nRevised code:\n", x$code, "\n", sep = "")
  invisible(x)
}

#' Compare the final objects of two traces
#'
#' Use after re-running revised code to see, object by object, what the fix
#' changed and how many issues remain.
#'
#' @param old,new `rewind_trace` objects.
#' @return A data frame with one row per data frame present in both traces,
#'   with an `issues` attribute counting warnings and errors before and after.
#' @examples
#' old <- rewind_script(text = c(
#'   "d <- data.frame(x = c('1', '2', '$3'))",
#'   "d$x <- as.numeric(d$x)"))
#' new <- rewind_script(text = c(
#'   "d <- data.frame(x = c('1', '2', '$3'))",
#'   "d$x <- as.numeric(gsub('[$]', '', d$x))"))
#' compare_traces(old, new)
#' @export
compare_traces <- function(old, new) {
  objs <- intersect(names(old$versions), names(new$versions))
  rows <- lapply(objs, function(o) {
    a <- at_step(old, o); b <- at_step(new, o)
    if (!is_df(a) || !is_df(b)) return(NULL)
    d <- df_diff(a, b, key = old$key)
    data.frame(object = o, rows_old = nrow(a), rows_new = nrow(b),
               cols_added = paste(d$cols_added, collapse = ", "),
               cols_removed = paste(d$cols_removed, collapse = ", "),
               na_old = sum(na_counts(a)), na_new = sum(na_counts(b)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "issues") <- c(old = nrow(issues(old)), new = nrow(issues(new)))
  out
}
