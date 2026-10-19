# State summaries and diffs ------------------------------------------------

is_df <- function(x) is.data.frame(x)

col_class <- function(x) class(x)[1]

na_counts <- function(df) {
  vapply(df, function(col) {
    if (is.list(col)) sum(vapply(col, function(v) length(v) == 0 || all(is.na(v)), logical(1)))
    else sum(is.na(col))
  }, numeric(1))
}

# Compact description of any value, used for the timeline and the UI.
summarise_value <- function(x) {
  if (is_df(x)) {
    list(
      type = "data.frame", class = class(x)[1],
      nrow = nrow(x), ncol = ncol(x), names = names(x),
      classes = vapply(x, col_class, character(1)),
      na = na_counts(x),
      bytes = as.numeric(utils::object.size(x))
    )
  } else {
    txt <- tryCatch(utils::capture.output(utils::str(x, max.level = 1, vec.len = 3, give.attr = FALSE)),
                    error = function(e) class(x)[1])
    list(type = "other", class = class(x)[1], length = length(x),
         preview = paste(utils::head(txt, 3), collapse = "\n"),
         bytes = as.numeric(utils::object.size(x)))
  }
}

key_strings <- function(df, key) {
  if (length(key) == 1) return(as.character(df[[key]]))
  do.call(paste, c(lapply(df[key], as.character), sep = "\r"))
}

cells_differ <- function(a, b) {
  if (is.factor(a)) a <- as.character(a)
  if (is.factor(b)) b <- as.character(b)
  if (is.list(a) || is.list(b)) {
    return(!mapply(identical, a, b, USE.NAMES = FALSE))
  }
  na_a <- is.na(a)
  na_b <- is.na(b)
  out <- xor(na_a, na_b)
  both <- !na_a & !na_b
  cmp <- tryCatch(a[both] != b[both], error = function(e) rep(TRUE, sum(both)))
  out[both] <- cmp
  out
}

#' Compare two data frames
#'
#' Computes what changed between two states of a data frame: row counts,
#' added/removed columns, type changes, new missing values and (when the
#' rows can be aligned by position or by `key`) which cells changed.
#'
#' @param before,after Data frames.
#' @param key Optional character vector of columns that identify a row.
#' @param max_cells Skip cell-level comparison above this many cells.
#' @return An object of class `rewind_diff`.
#' @examples
#' before <- data.frame(id = 1:4, x = c("1", "2", "3", "n/a"))
#' after <- transform(before, x = suppressWarnings(as.numeric(x)), y = x > 1)
#' df_diff(before, after)
#' df_diff(before, after[-2, ], key = "id")
#' @export
df_diff <- function(before, after, key = NULL, max_cells = 5e6) {
  stopifnot(is_df(before), is_df(after))
  nb <- nrow(before); na <- nrow(after)
  common <- intersect(names(before), names(after))
  cls_b <- vapply(before[common], col_class, character(1))
  cls_a <- vapply(after[common], col_class, character(1))
  type_changes <- data.frame(column = common[cls_b != cls_a],
                             from = unname(cls_b[cls_b != cls_a]),
                             to = unname(cls_a[cls_b != cls_a]),
                             stringsAsFactors = FALSE)
  na_b <- na_counts(before); na_a <- na_counts(after)
  all_after <- names(after)
  before_na <- ifelse(all_after %in% names(na_b), na_b[all_after], NA)
  na_delta <- data.frame(column = all_after,
                         before = as.numeric(before_na),
                         after = as.numeric(na_a[all_after]),
                         stringsAsFactors = FALSE)
  na_delta$new_na <- na_delta$after - ifelse(is.na(na_delta$before), 0, na_delta$before)
  na_delta$pct_after <- if (na > 0) na_delta$after / na else 0
  na_delta <- na_delta[na_delta$new_na > 0, , drop = FALSE]
  rownames(na_delta) <- NULL

  out <- list(
    rows_before = nb, rows_after = na, row_delta = na - nb,
    row_pct = if (nb > 0) (na - nb) / nb else NA_real_,
    cols_added = setdiff(names(after), names(before)),
    cols_removed = setdiff(names(before), names(after)),
    type_changes = type_changes, na_delta = na_delta,
    aligned_by = "none", changed_cells = integer(0), changed_rows = integer(0),
    dropped_keys = character(0), added_keys = character(0)
  )

  compare_cols <- common[cls_b == cls_a | (cls_b %in% c("integer", "numeric") & cls_a %in% c("integer", "numeric"))]

  if (!is.null(key) && all(key %in% names(before)) && all(key %in% names(after))) {
    kb <- key_strings(before, key); ka <- key_strings(after, key)
    out$aligned_by <- "key"
    out$dropped_keys <- setdiff(unique(kb), ka)
    out$added_keys <- setdiff(unique(ka), kb)
    if (!anyDuplicated(kb) && !anyDuplicated(ka)) {
      idx_b <- match(ka, kb)
      keep <- which(!is.na(idx_b))
      if (length(keep) * length(compare_cols) <= max_cells && length(keep) > 0) {
        changed <- lapply(setdiff(compare_cols, key), function(cc)
          cells_differ(before[[cc]][idx_b[keep]], after[[cc]][keep]))
        names(changed) <- setdiff(compare_cols, key)
        out$changed_cells <- vapply(changed, sum, numeric(1))
        if (length(changed)) out$changed_rows <- keep[Reduce(`|`, changed)]
      }
    } else {
      out$aligned_by <- "key (not unique)"
    }
  } else if (nb == na && nb * length(compare_cols) <= max_cells && nb > 0) {
    out$aligned_by <- "position"
    changed <- lapply(compare_cols, function(cc) cells_differ(before[[cc]], after[[cc]]))
    names(changed) <- compare_cols
    out$changed_cells <- vapply(changed, sum, numeric(1))
    if (length(changed)) out$changed_rows <- which(Reduce(`|`, changed))
  }
  out$changed_cells <- out$changed_cells[out$changed_cells > 0]
  structure(out, class = "rewind_diff")
}

#' @export
print.rewind_diff <- function(x, ...) {
  pct <- if (is.na(x$row_pct)) "" else sprintf(" (%+.1f%%)", 100 * x$row_pct)
  cat(sprintf("Rows: %s -> %s%s\n", fmt_n(x$rows_before), fmt_n(x$rows_after), pct))
  if (length(x$cols_added)) cat("Columns added:   ", paste(x$cols_added, collapse = ", "), "\n")
  if (length(x$cols_removed)) cat("Columns removed: ", paste(x$cols_removed, collapse = ", "), "\n")
  if (nrow(x$type_changes)) {
    cat("Type changes:\n")
    for (i in seq_len(nrow(x$type_changes)))
      cat(sprintf("  %s: %s -> %s\n", x$type_changes$column[i], x$type_changes$from[i], x$type_changes$to[i]))
  }
  if (nrow(x$na_delta)) {
    cat("New missing values:\n")
    for (i in seq_len(nrow(x$na_delta)))
      cat(sprintf("  %s: +%s NA (%.1f%% of rows now NA)\n", x$na_delta$column[i],
                  fmt_n(x$na_delta$new_na[i]), 100 * x$na_delta$pct_after[i]))
  }
  if (length(x$changed_cells)) {
    cat(sprintf("Changed values (aligned by %s): %s rows\n", x$aligned_by, fmt_n(length(x$changed_rows))))
    for (nm in names(x$changed_cells)) cat(sprintf("  %s: %s cells\n", nm, fmt_n(x$changed_cells[[nm]])))
  }
  if (length(x$dropped_keys)) cat(sprintf("Keys dropped: %s (e.g. %s)\n", fmt_n(length(x$dropped_keys)),
                                         paste(utils::head(x$dropped_keys, 5), collapse = ", ")))
  if (length(x$added_keys)) cat(sprintf("Keys added: %s\n", fmt_n(length(x$added_keys))))
  invisible(x)
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

# Join diagnostics ---------------------------------------------------------

.join_funs <- c("left_join", "inner_join", "right_join", "full_join", "semi_join",
                "anti_join", "merge", "nest_join")

is_join_fun <- function(fun) !is.na(fun) && fun %in% .join_funs

resolve_join_by <- function(x, y, by, by_x = NULL, by_y = NULL) {
  if (!is.null(by_x) && !is.null(by_y)) return(list(x = by_x, y = by_y))
  if (is.null(by)) {
    k <- intersect(names(x), names(y))
    return(list(x = k, y = k))
  }
  if (inherits(by, "dplyr_join_by")) return(list(x = by$x, y = by$y))
  if (is.character(by)) {
    nm <- names(by)
    if (is.null(nm)) return(list(x = unname(by), y = unname(by)))
    bx <- ifelse(nzchar(nm), nm, by)
    return(list(x = unname(bx), y = unname(by)))
  }
  NULL
}

# Explains how well `x` matches `y` on the join keys.
join_report <- function(x, y, by) {
  bx <- by$x; byy <- by$y
  if (!length(bx) || !all(bx %in% names(x)) || !all(byy %in% names(y))) return(NULL)
  kx <- key_strings(x, bx); ky <- key_strings(y, byy)
  matched <- kx %in% ky
  unmatched_keys <- unique(kx[!matched & !is.na(kx)])
  norm <- function(k) tolower(trimws(gsub("\\s+", " ", k)))
  near <- unmatched_keys[norm(unmatched_keys) %in% norm(ky)]
  dup_y <- unique(ky[duplicated(ky)])
  type_x <- vapply(x[bx], col_class, character(1))
  type_y <- vapply(y[byy], col_class, character(1))
  list(
    by_x = bx, by_y = byy,
    rows_x = nrow(x), rows_y = nrow(y),
    unmatched_rows = sum(!matched),
    unmatched_pct = if (nrow(x)) mean(!matched) else 0,
    unmatched_examples = utils::head(unmatched_keys, 8),
    near_miss_keys = utils::head(near, 8), n_near_miss = length(near),
    na_keys = sum(is.na(kx)),
    dup_y_keys = length(dup_y),
    fanout_rows = sum(kx %in% dup_y),
    dup_y_examples = utils::head(dup_y, 5),
    key_type_mismatch = any(type_x != type_y),
    types = paste0(bx, " <", type_x, "> vs ", byy, " <", type_y, ">")
  )
}

# Evaluates the y/by arguments of a join stage to build a join_report.
analyse_join <- function(stage, input, env) {
  cl <- stage_effective_call(stage)
  if (is.null(cl) || !is_df(input)) return(NULL)
  e <- new.env(parent = env)
  assign(".rw_in", input, envir = e)
  fn <- tryCatch(eval(cl[[1]], e), error = function(err) NULL)
  if (!is.function(fn)) return(NULL)
  mc <- tryCatch(match.call(fn, cl, expand.dots = TRUE), error = function(err) cl)
  arg <- function(name) {
    a <- mc[[name]]
    if (is.null(a)) return(NULL)
    tryCatch(eval(a, e), error = function(err) NULL)
  }
  y <- arg("y")
  if (is.null(y) && length(cl) >= 3 && (is.null(names(cl)) || !nzchar(names(cl)[3])))
    y <- tryCatch(eval(cl[[3]], e), error = function(err) NULL)
  if (!is_df(y)) return(NULL)
  by <- resolve_join_by(input, y, arg("by"), arg("by.x"), arg("by.y"))
  if (is.null(by)) return(NULL)
  rep <- join_report(input, y, by)
  if (!is.null(rep)) rep$type <- stage$fun
  rep
}
