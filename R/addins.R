# RStudio addins -----------------------------------------------------------

.rewindr_state <- new.env(parent = emptyenv())

#' The most recent trace recorded by an RStudio addin
#'
#' The addins keep their trace inside the package, so you can carry on in
#' the console after closing the app.
#'
#' @return A `rewind_trace`, or `NULL` if no addin has run in this session.
#' @examples
#' last_trace()
#' @export
last_trace <- function() .rewindr_state$last

# Opens the scrubber in RStudio's viewer pane (maximised) when available.
scrub_in_rstudio <- function(tr) {
  .rewindr_state$last <- tr
  print(tr)
  message("Use last_trace() to keep working with this trace in the console.")
  viewer <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    shiny::paneViewer(minHeight = "maximize")
  } else TRUE
  scrub(tr, launch.browser = viewer)
}

#' RStudio addins
#'
#' `addin_rewind_script()` records the script open in the editor (unsaved
#' changes included) and `addin_rewind_selection()` records the selected
#' code. Both evaluate against the global environment, inside a sandbox, and
#' open the time-travel app. Run them from RStudio's Addins menu.
#'
#' @return Called for their side effect of opening the app; return `NULL`
#'   invisibly. The trace is available afterwards from [last_trace()].
#' @examples
#' if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
#'     rstudioapi::isAvailable()) {
#'   addin_rewind_script()
#' }
#' @name addins
NULL

#' @rdname addins
#' @export
addin_rewind_script <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  text <- ctx$contents
  if (!length(text) || !any(nzchar(trimws(text)))) stop("The active editor is empty.", call. = FALSE)
  if (nzchar(ctx$path)) {
    old <- setwd(dirname(normalizePath(ctx$path)))
    on.exit(setwd(old), add = TRUE)
  }
  tr <- rewind_script(text = text, env = globalenv())
  if (nzchar(ctx$path)) tr$file <- ctx$path
  scrub_in_rstudio(tr)
  invisible(NULL)
}

#' @rdname addins
#' @export
addin_rewind_selection <- function() {
  ctx <- rstudioapi::getSourceEditorContext()
  text <- ctx$selection[[1]]$text
  if (!nzchar(trimws(text))) stop("Select some code first (e.g. a pipeline).", call. = FALSE)
  tr <- rewind_script(text = text, env = globalenv())
  scrub_in_rstudio(tr)
  invisible(NULL)
}
