#' rewindr: Time-Travel Debugging for Data Pipelines
#'
#' rewindr records the data after every step of a pipeline or script, finds
#' the first step where it silently went wrong, and lets you scrub backward
#' and forward through every state.
#'
#' Start with [rewind()] for a pipeline or [rewind_script()] for a script,
#' then read the result with [issues()], [blame()] and [step_diff()], or open
#' the interactive timeline with [scrub()]. See `vignette("rewindr")`.
#'
#' @keywords internal
"_PACKAGE"

#' Path to the bundled demo pipeline
#'
#' A short sales pipeline with three planted bugs: amounts with a currency
#' symbol that become `NA`, customer ids padded with whitespace and
#' lower-cased so an inner join drops rows, and duplicated customers that
#' multiply rows. Running it needs 'dplyr'.
#'
#' @return The path to `broken_pipeline.R` inside the installed package.
#' @examples
#' rewind_example()
#' cat(readLines(rewind_example()), sep = "\n")
#' @export
rewind_example <- function() {
  system.file("examples", "broken_pipeline.R", package = "rewindr", mustWork = TRUE)
}
