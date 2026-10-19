## Submission

This is a new submission.

rewindr records the data after every step of a data pipeline or script,
compares consecutive states, and reports the first step where the data
silently went wrong (lost or duplicated rows, new missing values, join keys
that do not match). A 'shiny' app lets the user move backward and forward
through the recorded states.

## Test environments

* Local: Windows 11, R 4.6.1 and R 4.6.0
* GitHub Actions: Windows (release), macOS (release), Ubuntu (release and
  oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes for the reviewer

* `ask_claude()` calls a paid web API and needs an API key, so its example
  is wrapped in `\dontrun{}`. Everything else, including the tests and the
  vignette, runs offline.
* `scrub()` and the RStudio addins open an interactive 'shiny' app, so their
  examples only run when `interactive()` is true.
* Code evaluated by `rewind()` and `rewind_script()` runs in a new
  environment, so the user's workspace and the global environment are not
  modified. `rewind_script()` and the addin change the working directory to
  the script's folder only while the script runs and restore it with
  `on.exit()`.
* `containerize()` writes only to the directory given in `out_dir`; the
  example and tests use `tempdir()`.
