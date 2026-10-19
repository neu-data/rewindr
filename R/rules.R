# Anomaly rules ------------------------------------------------------------

#' Expectations used to flag suspicious steps
#'
#' @param max_row_loss Flag a step that drops more than this share of rows
#'   (0.05 = 5%). Filters are expected to drop rows, so they are reported
#'   at "info" level below `filter_row_loss`.
#' @param filter_row_loss Row-loss share above which even a filter is flagged.
#' @param flag_join_fanout Flag joins that add rows because of duplicate keys.
#' @param max_join_unmatched Flag joins where more than this share of the
#'   left-hand rows found no match.
#' @param flag_new_na Flag columns that gain missing values.
#' @param flag_type_change Flag columns whose class changes.
#' @param flag_empty Flag steps that return zero rows.
#' @param max_row_gain Optional: flag any non-join step that grows the row
#'   count by more than this share. `NULL` disables.
#' @return A list of class `rewind_rules`.
#' @examples
#' strict <- rewind_rules(max_row_loss = 0, max_join_unmatched = 0)
#' tr <- rewind(mtcars |> subset(mpg > 25), rules = strict)
#' issues(tr)
#' @export
rewind_rules <- function(max_row_loss = 0.05, filter_row_loss = 0.9,
                         flag_join_fanout = TRUE, max_join_unmatched = 0.01,
                         flag_new_na = TRUE, flag_type_change = TRUE,
                         flag_empty = TRUE, max_row_gain = NULL) {
  structure(as.list(environment()), class = "rewind_rules")
}

.filter_funs <- c("filter", "subset", "slice", "slice_head", "slice_tail", "slice_sample",
                  "slice_min", "slice_max", "head", "tail", "distinct", "unique",
                  "drop_na", "na.omit", "semi_join", "anti_join", "[", "top_n", "sample_n")

# Steps that are expected to change the row count completely.
.reshape_funs <- c("summarise", "summarize", "count", "tally", "aggregate", "reframe",
                   "pivot_longer", "pivot_wider", "spread", "gather", "melt", "dcast",
                   "unnest", "separate_rows", "table", "tapply", "bind_rows", "rbind",
                   "complete", "expand", "crossing", "uncount")

issue <- function(severity, type, message, hint = NA_character_) {
  data.frame(severity = severity, type = type, message = message, hint = hint,
             stringsAsFactors = FALSE)
}

# Returns a data frame of issues for one step, given its diff/join report.
check_step <- function(step, prev, rules, key = NULL) {
  out <- list()
  add <- function(...) out[[length(out) + 1]] <<- issue(...)

  if (!is.null(step$error)) {
    add("error", "error", paste0("Step failed: ", step$error),
        "Rewind to the previous step to inspect the input this code received.")
  }
  for (w in step$warnings) {
    hint <- if (grepl("coercion", w)) "Values could not be parsed; inspect them before converting (e.g. readr::parse_number or a regex clean-up)." else NA_character_
    add("warning", "r_warning", paste0("R warning: ", w), hint)
  }

  v <- step$value
  if (is_df(v) && isTRUE(rules$flag_empty) && nrow(v) == 0 && (is.null(prev) || !is_df(prev$value) || nrow(prev$value) > 0)) {
    add("error", "empty", "Result has zero rows.", "Check the filter/join conditions of this step.")
  }

  j <- step$join
  if (!is.null(j)) {
    if (isTRUE(j$key_type_mismatch)) {
      add("error", "join_key_type", paste0("Join key types differ: ", paste(j$types, collapse = "; ")),
          "Convert the keys to the same type before joining.")
    }
    if (j$unmatched_pct > rules$max_join_unmatched && !j$type %in% c("anti_join", "semi_join")) {
      verb <- if (j$type %in% c("inner_join", "merge")) "were dropped" else "got no match (their new columns are NA)"
      msg <- sprintf("%s of %s left-hand rows (%.1f%%) %s in %s on [%s].",
                     fmt_n(j$unmatched_rows), fmt_n(j$rows_x), 100 * j$unmatched_pct, verb, j$type,
                     paste(j$by_x, collapse = ", "))
      hint <- if (j$n_near_miss > 0) {
        sprintf("%s unmatched keys match after trimming whitespace / ignoring case (e.g. '%s'). Normalise the keys with trimws()/toupper() before the join.",
                fmt_n(j$n_near_miss), j$near_miss_keys[1])
      } else if (length(j$unmatched_examples)) {
        sprintf("Example unmatched keys: %s. Use anti_join() to list them.",
                paste(shQuote(utils::head(j$unmatched_examples, 3), "sh"), collapse = ", "))
      } else NA_character_
      add(if (j$unmatched_pct >= 0.1 || j$n_near_miss > 0) "warning" else "info", "join_unmatched", msg, hint)
    }
    if (isTRUE(rules$flag_join_fanout) && j$dup_y_keys > 0 && j$fanout_rows > 0 &&
        !j$type %in% c("anti_join", "semi_join")) {
      add("warning", "join_fanout",
          sprintf("Right-hand table has %s duplicated key(s) (e.g. %s); %s left rows were multiplied.",
                  fmt_n(j$dup_y_keys), paste0("'", utils::head(j$dup_y_examples, 3), "'", collapse = ", "), fmt_n(j$fanout_rows)),
          "De-duplicate the lookup table on its key (distinct(y, key, .keep_all = TRUE)) or add the missing key columns to `by`.")
    }
  }

  d <- step$diff
  if (!is.null(d)) {
    fun <- step$fun
    is_filter <- !is.na(fun) && fun %in% .filter_funs
    is_join <- is_join_fun(fun)
    is_reshape <- !is.na(fun) && fun %in% .reshape_funs
    loss <- -d$row_pct
    if (is_filter && !is.na(loss) && loss > 0 && is_df(prev$value) && length(step$vars)) {
      cols <- intersect(step$vars, names(prev$value))
      if (length(cols)) {
        na_rows <- Reduce(`|`, lapply(prev$value[cols], is.na))
        n_na <- sum(na_rows)
        if (n_na > 0 && n_na <= -d$row_delta) {
          add("warning", "filter_drops_na",
              sprintf("%s of the removed rows were dropped only because [%s] is NA, not because of the condition.",
                      fmt_n(n_na), paste(cols[vapply(prev$value[cols], anyNA, logical(1))], collapse = ", ")),
              "Missing values silently fail the filter condition. Fix them upstream, or keep them explicitly (e.g. `| is.na(col)`).")
        }
      }
    }
    if (!is.na(loss) && loss > 0 && !is_join && !is_reshape) {
      if (is_filter && loss <= rules$filter_row_loss) {
        add("info", "row_loss", sprintf("Filter removed %s rows (%.1f%%).", fmt_n(-d$row_delta), 100 * loss))
      } else if (loss > rules$max_row_loss) {
        add("warning", "row_loss", sprintf("Lost %s of %s rows (%.1f%%).", fmt_n(-d$row_delta), fmt_n(d$rows_before), 100 * loss),
            if (is_filter) "This filter removed more rows than expected; check its condition against NA values and data types." else NA_character_)
      }
    }
    if (!is_join && !is_reshape && !is.null(rules$max_row_gain) && !is.na(d$row_pct) && d$row_pct > rules$max_row_gain) {
      add("warning", "row_gain", sprintf("Row count grew by %.1f%%.", 100 * d$row_pct))
    }
    if (is_join && is.null(j) && !is.na(d$row_pct) && d$row_pct > 0 && fun != "full_join") {
      add("warning", "join_fanout", sprintf("Join increased rows by %s (%.1f%%).", fmt_n(d$row_delta), 100 * d$row_pct),
          "The lookup table probably has duplicated keys.")
    }
    if (isTRUE(rules$flag_type_change) && nrow(d$type_changes)) {
      for (i in seq_len(nrow(d$type_changes))) {
        tc <- d$type_changes[i, ]
        add("info", "type_change", sprintf("Column '%s' changed type: %s -> %s.", tc$column, tc$from, tc$to))
      }
    }
    if (isTRUE(rules$flag_new_na) && nrow(d$na_delta)) {
      for (i in seq_len(nrow(d$na_delta))) {
        nd <- d$na_delta[i, ]
        sev <- if (nd$column %in% d$cols_added && !is_join) "info" else "warning"
        if (is_join && !is.null(j) && j$unmatched_pct > 0 && nd$column %in% d$cols_added) next
        add(sev, "new_na", sprintf("Column '%s' gained %s missing values (now %.1f%% NA).", nd$column, fmt_n(nd$new_na), 100 * nd$pct_after),
            if (nd$column %in% d$type_changes$column) "The type conversion could not parse some values." else NA_character_)
      }
    }
    if (!is.null(key) && is_df(v) && all(key %in% names(v)) && anyDuplicated(key_strings(v, key)) &&
        is_df(prev$value) && all(key %in% names(prev$value)) && !anyDuplicated(key_strings(prev$value, key))) {
      add("warning", "key_duplicated", sprintf("Key [%s] is no longer unique.", paste(key, collapse = ", ")))
    }
  }

  if (!length(out)) return(NULL)
  do.call(rbind, out)
}
