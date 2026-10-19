# rewindr 0.1.0

* First release, built on the Neudata R package template.
* `rewind()` and `rewind_script()` record the data after every step of a
  pipeline or script. Native and magrittr pipes are split into stages,
  in-place edits become new versions of an object, code runs in a sandbox,
  and errors are recorded instead of thrown.
* `df_diff()` and `step_diff()` compare states: rows, columns, types, new
  missing values and changed cells, aligned by position or by a key.
* Join checks report unmatched rows, keys that differ only in whitespace or
  case, duplicated lookup keys that multiply rows, and key type mismatches.
* `issues()`, `first_divergence()`, `lineage()` and `blame()` find where the
  data went wrong; `rewind_rules()` tunes the checks.
* `scrub()` opens a 'shiny' app to move backward and forward through every
  state, with the source line, the data, the diff and the join report.
* `suggest_fixes()` proposes code fixes offline; `ask_claude()` asks the
  'Claude' API for a diagnosis and a revised script, and
  `compare_traces()` compares a re-run with the original.
* `containerize()` and `script_packages()` turn a script into a 'Docker'
  recipe with pinned package versions.
* RStudio addins record the current script or the selected code.
