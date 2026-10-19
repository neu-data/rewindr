# Interactive time-travel UI -----------------------------------------------

.sev_col <- c(ok = "#198754", info = "#0d6efd", warning = "#d97706", error = "#dc3545")

#' Scrub through a recorded trace
#'
#' Opens an interactive timeline: drag the slider (or use the arrow keys, or
#' press play) to move backward and forward through every recorded state,
#' see what changed at each step, inspect the data, read the diagnosis, and
#' ask Claude for a fix that is re-run in a sandbox and compared with the
#' original.
#'
#' @param tr A `rewind_trace`.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Further arguments for [shiny::runApp()].
#' @return `scrub()` runs the app until it is closed and returns `NULL`
#'   invisibly; `scrub_app()` returns a `shiny.appobj`.
#' @examples
#' if (interactive()) {
#'   tr <- rewind_script(rewind_example(), key = "order_id")
#'   scrub(tr)
#' }
#' @export
scrub <- function(tr, launch.browser = interactive(), ...) {
  shiny::runApp(scrub_app(tr), launch.browser = launch.browser, ...)
  invisible(NULL)
}

#' @rdname scrub
#' @export
scrub_app <- function(tr) {
  for (p in c("shiny", "bslib", "DT")) {
    if (!requireNamespace(p, quietly = TRUE)) stop("scrub() needs the '", p, "' package.", call. = FALSE)
  }
  stopifnot(inherits(tr, "rewind_trace"), length(tr$steps) > 0)
  shiny::shinyApp(scrub_ui(tr), scrub_server(tr))
}

stat_tile <- function(label, id) {
  shiny::tags$div(class = "rw-stat", shiny::tags$div(class = "lbl", label),
                  shiny::tags$div(class = "val", shiny::textOutput(id, inline = TRUE)))
}

scrub_ui <- function(tr) {
  tags <- shiny::tags
  css <- "
    .rw-step { display:flex; gap:.5rem; align-items:flex-start; padding:.35rem .5rem; border-radius:.4rem;
               cursor:pointer; font-size:.82rem; border-left:4px solid transparent; }
    .rw-step:hover { background: var(--bs-tertiary-bg); }
    .rw-step.active { background: var(--bs-primary-bg-subtle); }
    .rw-step code { white-space: pre-wrap; word-break: break-word; color: var(--bs-body-color); background:none; }
    .rw-num { min-width:1.8rem; color: var(--bs-secondary-color); font-variant-numeric: tabular-nums; }
    .rw-shape { margin-left:auto; color: var(--bs-secondary-color); white-space:nowrap; font-variant-numeric: tabular-nums; }
    .rw-code { font-family: var(--bs-font-monospace); font-size:.8rem; max-height: 420px; overflow:auto;
               background: var(--bs-tertiary-bg); border-radius:.4rem; padding:.5rem 0; }
    .rw-line { white-space: pre; padding: 0 .75rem; }
    .rw-line .ln { display:inline-block; width:2.5rem; color: var(--bs-secondary-color); user-select:none; }
    .rw-line.hl { background: rgba(255, 193, 7, .25); }
    .rw-line.hl-err { background: rgba(220, 53, 69, .2); }
    .rw-issue { border-left:4px solid; padding:.5rem .75rem; margin-bottom:.5rem; background: var(--bs-tertiary-bg); border-radius:.3rem; }
    .rw-chip { display:inline-block; padding:.1rem .5rem; border-radius:1rem; font-size:.78rem; margin:.1rem;
               background: var(--bs-secondary-bg); }
    .rw-chip.add { background: rgba(25,135,84,.15); } .rw-chip.del { background: rgba(220,53,69,.15); }
    .rw-stats { display:grid; grid-template-columns: repeat(auto-fit, minmax(140px, 1fr)); gap:.75rem; }
    .rw-stat { border:1px solid var(--bs-border-color); border-radius:.5rem; padding:.6rem .9rem; background: var(--bs-body-bg); }
    .rw-stat .lbl { font-size:.75rem; color: var(--bs-secondary-color); text-transform: uppercase; letter-spacing:.03em; }
    .rw-stat .val { font-size:1.35rem; font-weight:600; font-variant-numeric: tabular-nums; }
    .rw-scrub .irs-single, .rw-scrub .irs-min, .rw-scrub .irs-max, .rw-scrub .irs-from, .rw-scrub .irs-to { display:none; }
    .rw-scrub .irs { margin-top: -.5rem; }
    .rw-scrub .form-group { margin-bottom: 0; width: 100%; }
    pre.rw-fix { max-height: 380px; overflow: auto; font-size: .8rem; }
  "
  js <- "
    document.addEventListener('keydown', function(e) {
      var t = e.target.tagName;
      if (t === 'INPUT' || t === 'TEXTAREA' || t === 'SELECT') return;
      if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') { Shiny.setInputValue('rw_key', 'prev', {priority: 'event'}); e.preventDefault(); }
      if (e.key === 'ArrowRight' || e.key === 'ArrowDown') { Shiny.setInputValue('rw_key', 'next', {priority: 'event'}); e.preventDefault(); }
    });
    $(document).on('click', '.rw-step', function() {
      Shiny.setInputValue('rw_pick', parseInt($(this).data('step')), {priority: 'event'});
    });
  "
  bslib::page_sidebar(
    title = tags$span(tags$b("rewindr"), tags$span(class = "text-secondary ms-2 fs-6", "time-travel debugger")),
    theme = bslib::bs_theme(version = 5, primary = "#4f46e5"),
    fillable = FALSE,
    tags$head(tags$style(css), tags$script(shiny::HTML(js))),
    sidebar = bslib::sidebar(
      width = 400,
      shiny::selectInput("rw_object", "Follow an object", choices = NULL),
      shiny::checkboxInput("rw_df_only", "Data frames only", TRUE),
      shiny::uiOutput("rw_steps")
    ),
    bslib::card(
      class = "rw-scrub",
      bslib::card_body(
        tags$div(
          class = "d-flex align-items-center gap-2",
          shiny::actionButton("rw_prev", shiny::icon("backward-step"), class = "btn-outline-secondary btn-sm", title = "Previous step (Left arrow)"),
          tags$div(class = "flex-grow-1",
                   shiny::sliderInput("rw_pos", NULL, min = 1, max = 2, value = 1, step = 1, ticks = FALSE,
                                      width = "100%", animate = shiny::animationOptions(interval = 1500))),
          shiny::actionButton("rw_next", shiny::icon("forward-step"), class = "btn-outline-secondary btn-sm", title = "Next step (Right arrow)"),
          shiny::actionButton("rw_jump", "First problem", icon = shiny::icon("bug"), class = "btn-outline-danger btn-sm")
        ),
        shiny::uiOutput("rw_step_title")
      )
    ),
    tags$div(
      class = "rw-stats mb-3",
      stat_tile("Rows", "rw_rows"), stat_tile("Columns", "rw_cols"),
      stat_tile("Change in rows", "rw_delta"), stat_tile("Issues at this step", "rw_nissues")
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Code"), shiny::uiOutput("rw_code")),
      bslib::navset_card_tab(
        id = "rw_tab",
        bslib::nav_panel("Issues & fixes", shiny::uiOutput("rw_issues")),
        bslib::nav_panel("What changed", shiny::uiOutput("rw_diff_ctrl"), shiny::uiOutput("rw_diff")),
        bslib::nav_panel("Data", shiny::uiOutput("rw_data_note"), DT::DTOutput("rw_data")),
        bslib::nav_panel("Join", shiny::uiOutput("rw_join")),
        bslib::nav_panel(
          "Ask Claude",
          shiny::textAreaInput("rw_instruction", NULL, width = "100%", rows = 3,
                               placeholder = "Optional: describe what you want, e.g. \"fill missing months with zero revenue\". Leave empty to fix the detected issues."),
          shiny::uiOutput("rw_key_ui"),
          tags$div(class = "d-flex gap-2 mb-2",
                   shiny::actionButton("rw_ask", "Ask Claude", icon = shiny::icon("wand-magic-sparkles"), class = "btn-primary"),
                   shiny::uiOutput("rw_rerun_ui", inline = TRUE)),
          shiny::uiOutput("rw_fix")
        )
      )
    )
  )
}

scrub_server <- function(tr0) {
  force(tr0)
  function(input, output, session) {
    tags <- shiny::tags
    tr_r <- shiny::reactiveVal(tr0)
    fix_r <- shiny::reactiveVal(NULL)
    rerun_r <- shiny::reactiveVal(NULL)

    tl_r <- shiny::reactive(timeline(tr_r()))
    fixes_r <- shiny::reactive(suggest_fixes(tr_r()))

    shiny::observe({
      tr <- tr_r()
      objs <- names(tr$versions)
      dfs <- objs[vapply(objs, function(o) is_df(at_step(tr, o)), logical(1))]
      shiny::updateSelectInput(session, "rw_object", choices = c("All steps" = "", dfs))
    })

    # Steps visible on the timeline, in order.
    visible <- shiny::reactive({
      tr <- tr_r(); tl <- tl_r()
      ids <- tl$step
      if (nzchar(input$rw_object %||% "")) ids <- lineage(tr, input$rw_object)
      if (isTRUE(input$rw_df_only)) {
        keep <- vapply(ids, function(i) is_df(tr$steps[[i]]$value) || !is.null(tr$steps[[i]]$error), logical(1))
        ids <- ids[keep]
      }
      if (!length(ids)) ids <- tl$step
      ids
    })

    shiny::observe({
      ids <- visible()
      fd <- first_divergence(tr_r())
      start <- if (!is.na(fd) && fd %in% ids) match(fd, ids) else 1
      shiny::updateSliderInput(session, "rw_pos", min = 1, max = max(2, length(ids)), value = start)
    })

    cur <- shiny::reactive({
      ids <- visible()
      pos <- min(max(1, input$rw_pos %||% 1), length(ids))
      ids[pos]
    })
    step_r <- shiny::reactive(tr_r()$steps[[cur()]])

    go <- function(delta) {
      ids <- visible()
      pos <- min(max(1, (input$rw_pos %||% 1) + delta), length(ids))
      shiny::updateSliderInput(session, "rw_pos", value = pos)
    }
    shiny::observeEvent(input$rw_prev, go(-1))
    shiny::observeEvent(input$rw_next, go(1))
    shiny::observeEvent(input$rw_key, go(if (input$rw_key == "prev") -1 else 1))
    shiny::observeEvent(input$rw_pick, {
      pos <- match(input$rw_pick, visible())
      if (!is.na(pos)) shiny::updateSliderInput(session, "rw_pos", value = pos)
    })
    shiny::observeEvent(input$rw_jump, {
      iss <- issues(tr_r(), "warning")
      ids <- visible()
      hit <- ids[ids %in% iss$step]
      if (length(hit)) shiny::updateSliderInput(session, "rw_pos", value = match(hit[1], ids))
      else shiny::showNotification("No problems found on this timeline.", type = "message")
    })

    output$rw_steps <- shiny::renderUI({
      tl <- tl_r(); ids <- visible(); now <- cur()
      items <- lapply(ids, function(i) {
        r <- tl[tl$step == i, ]
        shape <- if (is.na(r$rows)) r$class else sprintf("%s\u00d7%s", fmt_n(r$rows), r$cols)
        delta <- if (!is.na(r$row_delta) && r$row_delta != 0) sprintf(" (%+d)", r$row_delta) else ""
        tags$div(class = paste("rw-step", if (i == now) "active"), `data-step` = i,
                 style = sprintf("border-left-color:%s", if (r$status == "ok") "transparent" else .sev_col[[r$status]]),
                 tags$span(class = "rw-num", i),
                 tags$code(r$label),
                 tags$span(class = "rw-shape", paste0(shape, delta)))
      })
      tags$div(tags$div(class = "small text-secondary mb-1", sprintf("%d steps", length(ids))), items)
    })

    output$rw_step_title <- shiny::renderUI({
      s <- step_r()
      loc <- if (!is.na(s$line)) sprintf("line %d \u00b7 ", s$line) else ""
      tags$div(class = "small text-secondary mt-1",
               sprintf("Step %d of %d \u00b7 %s", s$id, length(tr_r()$steps), loc),
               tags$code(step_label(s)))
    })

    output$rw_rows <- shiny::renderText({
      sm <- step_r()$summary
      if (is.null(sm)) "error" else if (sm$type == "data.frame") fmt_n(sm$nrow) else sm$class
    })
    output$rw_cols <- shiny::renderText({
      sm <- step_r()$summary
      if (!is.null(sm) && sm$type == "data.frame") sm$ncol else "\u2013"
    })
    output$rw_delta <- shiny::renderText({
      d <- step_r()$diff
      if (is.null(d)) "\u2013" else sprintf("%+d (%+.1f%%)", d$row_delta, 100 * ifelse(is.na(d$row_pct), 0, d$row_pct))
    })
    output$rw_nissues <- shiny::renderText({
      iss <- step_r()$issues
      if (is.null(iss)) "0" else sprintf("%d (%s)", nrow(iss), worst_severity(iss$severity))
    })

    output$rw_code <- shiny::renderUI({
      tr <- tr_r(); s <- step_r()
      if (!is.na(s$line)) {
        src <- strsplit(tr$source, "\n", fixed = TRUE)[[1]]
        n_stmt <- length(strsplit(s$stmt_code, "\n", fixed = TRUE)[[1]])
        hl <- seq(s$line, length.out = n_stmt)
        cls <- if (!is.null(s$error)) "hl-err" else "hl"
        lines <- shiny::HTML(paste0(sprintf(
          "<div class=\"rw-line%s\" id=\"rwl%d\"><span class=\"ln\">%d</span>%s</div>",
          ifelse(seq_along(src) %in% hl, paste0(" ", cls), ""), seq_along(src), seq_along(src),
          htmltools::htmlEscape(src)), collapse = ""))
        scroll <- tags$script(shiny::HTML(sprintf(
          "setTimeout(function(){var el=document.getElementById('rwl%d'); if(el){var p=el.parentNode; p.scrollTop=el.offsetTop-p.offsetTop-40;}}, 50);", max(1, s$line - 1))))
        tags$div(tags$div(class = "rw-code", lines), scroll,
                 tags$div(class = "mt-2 small", tags$b("This step: "), tags$code(s$code)))
      } else {
        tags$div(tags$div(class = "rw-code", shiny::HTML(sprintf("<div class=\"rw-line hl\">%s</div>", htmltools::htmlEscape(s$stmt_code)))),
                 tags$div(class = "mt-2 small", tags$b("This step: "), tags$code(s$code)))
      }
    })

    output$rw_issues <- shiny::renderUI({
      s <- step_r(); fx <- fixes_r()
      iss <- s$issues
      blocks <- list()
      if (is.null(iss)) {
        blocks <- list(tags$p(class = "text-success", shiny::icon("circle-check"), " Nothing suspicious at this step."))
      } else {
        blocks <- lapply(seq_len(nrow(iss)), function(k) {
          tags$div(class = "rw-issue", style = sprintf("border-color:%s", .sev_col[[iss$severity[k]]]),
                   tags$div(tags$b(toupper(iss$severity[k])), tags$span(class = "text-secondary small ms-2", iss$type[k])),
                   tags$div(iss$message[k]),
                   if (!is.na(iss$hint[k])) tags$div(class = "small mt-1", shiny::icon("lightbulb"), " ", iss$hint[k]))
        })
      }
      mine <- fx[fx$step == s$id, , drop = FALSE]
      if (nrow(mine)) {
        blocks <- c(blocks, list(tags$h6(class = "mt-3", "Suggested fixes")),
                    lapply(seq_len(nrow(mine)), function(k)
                      tags$div(tags$div(class = "small", mine$problem[k]), tags$pre(class = "small", mine$suggestion[k]))))
      }
      all_iss <- issues(tr_r(), "warning")
      if (nrow(all_iss)) {
        steps_with <- unique(all_iss$step)
        blocks <- c(blocks, list(tags$hr(), tags$div(class = "small text-secondary",
          sprintf("Whole trace: %d warnings/errors at steps %s. First divergence: step %d.",
                  nrow(all_iss), paste(steps_with, collapse = ", "), min(steps_with)))))
      }
      tags$div(blocks)
    })

    output$rw_diff_ctrl <- shiny::renderUI({
      s <- step_r(); tr <- tr_r()
      dfs <- Filter(function(i) i != s$id && is_df(tr$steps[[i]]$value), seq_along(tr$steps))
      if (!is_df(s$value) || !length(dfs)) return(NULL)
      sel <- if (!is.na(s$input)) s$input else ""
      shiny::selectInput("rw_against", "Compare with step", width = "100%",
                         choices = c("(input of this step)" = "", stats::setNames(dfs, paste(dfs, vapply(dfs, function(i) step_label(tr$steps[[i]]), character(1))))),
                         selected = "")
    })

    diff_r <- shiny::reactive({
      s <- step_r()
      ag <- input$rw_against %||% ""
      if (nzchar(ag) && is_df(s$value)) df_diff(at_step(tr_r(), as.integer(ag)), s$value, key = tr_r()$key) else s$diff
    })

    output$rw_diff <- shiny::renderUI({
      d <- diff_r(); s <- step_r()
      if (is.null(d)) {
        if (!is.null(s$error)) return(tags$div(class = "text-danger", tags$b("Error: "), s$error))
        return(tags$p(class = "text-secondary", "No previous data frame state to compare with (this is where the data enters)."))
      }
      chips <- function(x, cls) if (length(x)) lapply(x, function(v) tags$span(class = paste("rw-chip", cls), v)) else tags$span(class = "text-secondary", "none")
      tbl <- function(df) {
        if (!nrow(df)) return(tags$p(class = "text-secondary small", "none"))
        tags$table(class = "table table-sm small",
                   tags$thead(tags$tr(lapply(names(df), tags$th))),
                   tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(df[i, ], function(v) tags$td(format(v, big.mark = ",")))))))
      }
      na_tbl <- d$na_delta
      if (nrow(na_tbl)) na_tbl$pct_after <- sprintf("%.1f%%", 100 * na_tbl$pct_after)
      cc <- d$changed_cells
      tags$div(
        tags$p(tags$b("Rows: "), sprintf("%s \u2192 %s", fmt_n(d$rows_before), fmt_n(d$rows_after)),
               if (!is.na(d$row_pct)) sprintf(" (%+.1f%%)", 100 * d$row_pct)),
        tags$p(tags$b("Columns added: "), chips(d$cols_added, "add")),
        tags$p(tags$b("Columns removed: "), chips(d$cols_removed, "del")),
        tags$h6(class = "mt-3", "Type changes"), tbl(d$type_changes),
        tags$h6(class = "mt-3", "New missing values"), tbl(na_tbl),
        tags$h6(class = "mt-3", sprintf("Changed values (rows aligned by %s)", d$aligned_by)),
        if (length(cc)) tbl(data.frame(column = names(cc), cells_changed = unname(cc))) else tags$p(class = "text-secondary small", if (d$aligned_by == "none") "Row counts differ, so rows cannot be aligned by position. Pass key= to rewind() for row-level lineage." else "none"),
        if (length(d$dropped_keys)) tags$p(class = "mt-2", tags$b("Keys dropped: "), fmt_n(length(d$dropped_keys)), " e.g. ", paste(utils::head(d$dropped_keys, 8), collapse = ", "))
      )
    })

    output$rw_data_note <- shiny::renderUI({
      s <- step_r(); d <- s$diff
      if (!is_df(s$value)) return(NULL)
      n <- nrow(s$value)
      msg <- sprintf("%s rows%s.", fmt_n(n), if (n > 5000) " (showing the first 5,000)" else "")
      if (!is.null(d) && length(d$changed_rows)) msg <- paste(msg, sprintf("%s rows changed at this step are highlighted;", fmt_n(length(d$changed_rows))))
      if (!is.null(d) && length(d$cols_added)) msg <- paste(msg, "new columns are tinted green.")
      tags$p(class = "small text-secondary", msg)
    })

    output$rw_data <- DT::renderDT({
      s <- step_r()
      v <- s$value
      if (!is_df(v)) {
        txt <- if (!is.null(s$error)) paste("Error:", s$error) else paste(utils::capture.output(utils::str(v)), collapse = "\n")
        return(DT::datatable(data.frame(value = txt), rownames = FALSE, options = list(dom = "t")))
      }
      v <- as.data.frame(utils::head(v, 5000))
      for (nm in names(v)) if (is.list(v[[nm]])) v[[nm]] <- vapply(v[[nm]], function(x) paste(format(x), collapse = ", "), character(1))
      d <- s$diff
      flag <- rep(0L, nrow(v))
      if (!is.null(d) && length(d$changed_rows)) flag[d$changed_rows[d$changed_rows <= nrow(v)]] <- 1L
      v$.rw_changed <- flag
      dt <- DT::datatable(v, rownames = FALSE, filter = "top", class = "compact stripe",
                          options = list(pageLength = 15, scrollX = TRUE,
                                         columnDefs = list(list(visible = FALSE, targets = ncol(v) - 1))))
      dt <- DT::formatStyle(dt, ".rw_changed", target = "row",
                            backgroundColor = DT::styleEqual(1, "rgba(255,193,7,.25)"))
      if (!is.null(d) && length(d$cols_added)) {
        dt <- DT::formatStyle(dt, intersect(d$cols_added, names(v)), backgroundColor = "rgba(25,135,84,.12)")
      }
      dt
    })

    output$rw_join <- shiny::renderUI({
      j <- step_r()$join
      if (is.null(j)) return(tags$p(class = "text-secondary", "This step is not a join (or its keys could not be resolved)."))
      row <- function(k, v) tags$tr(tags$th(k), tags$td(v))
      quote_keys <- function(x) if (length(x)) paste0("'", x, "'", collapse = ", ") else "none"
      tags$table(class = "table table-sm",
        row("Join", sprintf("%s on %s", j$type, paste(sprintf("%s = %s", j$by_x, j$by_y), collapse = ", "))),
        row("Key types", paste(j$types, collapse = "; ")),
        row("Left rows", fmt_n(j$rows_x)), row("Lookup rows", fmt_n(j$rows_y)),
        row("Left rows with no match", sprintf("%s (%.1f%%)", fmt_n(j$unmatched_rows), 100 * j$unmatched_pct)),
        row("Unmatched key examples", quote_keys(j$unmatched_examples)),
        row("Near misses (whitespace/case)", sprintf("%s: %s", fmt_n(j$n_near_miss), quote_keys(j$near_miss_keys))),
        row("Duplicated lookup keys", sprintf("%s: %s", fmt_n(j$dup_y_keys), quote_keys(j$dup_y_examples))),
        row("Left rows multiplied", fmt_n(j$fanout_rows)),
        row("Missing keys on the left", fmt_n(j$na_keys)))
    })

    # Claude -------------------------------------------------------------
    output$rw_key_ui <- shiny::renderUI({
      if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) return(NULL)
      shiny::passwordInput("rw_api_key", "Anthropic API key (kept for this session only)", width = "100%")
    })

    shiny::observeEvent(input$rw_ask, {
      key <- if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) Sys.getenv("ANTHROPIC_API_KEY") else input$rw_api_key %||% ""
      if (!nzchar(key)) {
        shiny::showNotification("Add an API key, or set ANTHROPIC_API_KEY before starting the app.", type = "error")
        return()
      }
      rerun_r(NULL)
      res <- shiny::withProgress(message = "Asking Claude...", value = 0.3, tryCatch(
        ask_claude(tr_r(), instruction = input$rw_instruction, step = cur(), api_key = key),
        error = function(e) e))
      if (inherits(res, "error")) {
        shiny::showNotification(conditionMessage(res), type = "error", duration = 10)
      } else fix_r(res)
    })

    output$rw_rerun_ui <- shiny::renderUI({
      if (is.null(fix_r())) return(NULL)
      shiny::actionButton("rw_rerun", "Re-run revised code in a sandbox", icon = shiny::icon("play"), class = "btn-success")
    })

    output$rw_fix <- shiny::renderUI({
      fx <- fix_r()
      if (is.null(fx)) {
        return(tags$p(class = "small text-secondary",
          "Claude gets the code, the recorded timeline, the issues found, and a few sample rows from the steps with issues. ",
          "It returns a diagnosis and a revised script. Nothing runs until you click Re-run, and then only in a sandbox."))
      }
      rr <- rerun_r()
      cmp <- NULL
      if (!is.null(rr)) {
        if (inherits(rr, "error")) {
          cmp <- tags$div(class = "text-danger", "Re-run failed: ", conditionMessage(rr))
        } else {
          tab <- compare_traces(tr_r(), rr)
          n_iss <- attr(tab, "issues")
          cmp <- tags$div(class = "alert alert-success mt-2",
            tags$b(sprintf("Re-run: %d warnings/errors before, %d after.", n_iss[["old"]], n_iss[["new"]])),
            if (!is.null(tab) && nrow(tab)) tags$table(class = "table table-sm small mt-2 mb-2",
              tags$thead(tags$tr(lapply(c("object", "rows before", "rows after", "NA before", "NA after"), tags$th))),
              tags$tbody(lapply(seq_len(nrow(tab)), function(i) tags$tr(
                tags$td(tab$object[i]), tags$td(fmt_n(tab$rows_old[i])), tags$td(fmt_n(tab$rows_new[i])),
                tags$td(fmt_n(tab$na_old[i])), tags$td(fmt_n(tab$na_new[i])))))),
            shiny::actionButton("rw_switch", "Scrub the new run", class = "btn-sm btn-outline-success"),
            shiny::downloadButton("rw_download", "Download revised script", class = "btn-sm btn-outline-secondary"))
        }
      }
      tags$div(
        tags$h6("Diagnosis"), tags$p(fx$diagnosis),
        tags$h6("Changes"), tags$ul(lapply(fx$changes, tags$li)),
        cmp,
        tags$h6("Revised code"), tags$pre(class = "rw-fix", fx$code)
      )
    })

    shiny::observeEvent(input$rw_rerun, {
      tr <- tr_r(); fx <- fix_r()
      res <- shiny::withProgress(message = "Re-running in a sandbox...", value = 0.5, tryCatch({
        if (!is.null(tr$file)) {
          old <- setwd(dirname(normalizePath(tr$file)))
          on.exit(setwd(old), add = TRUE)
        }
        new <- rewind_script(text = fx$code, key = tr$key, rules = tr$rules, env = tr$parent_env)
        new$file <- tr$file
        new
      }, error = function(e) e))
      rerun_r(res)
    })

    shiny::observeEvent(input$rw_switch, {
      rr <- rerun_r()
      if (inherits(rr, "rewind_trace")) {
        tr_r(rr); fix_r(NULL); rerun_r(NULL)
        shiny::showNotification("Now scrubbing the revised run.", type = "message")
      }
    })

    output$rw_download <- shiny::downloadHandler(
      filename = function() "revised_script.R",
      content = function(file) writeLines(fix_r()$code, file)
    )
  }
}
