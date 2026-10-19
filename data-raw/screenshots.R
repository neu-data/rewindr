# ---------------------------------------------------------------------------
# Takes the screenshots of the scrub() app used in the pkgdown articles.
#
# Starts the app on the bundled demo in a background R process, drives a
# headless Chrome/Edge with chromote, and writes PNGs to
# vignettes/articles/figures/.
#
# Run from the package root, with rewindr installed:
#   source("data-raw/screenshots.R")
# Needs callr and chromote. On Windows without Chrome, point CHROMOTE_CHROME
# at msedge.exe.
# ---------------------------------------------------------------------------

if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
  edge <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  if (file.exists(edge)) Sys.setenv(CHROMOTE_CHROME = edge)
}

port <- 7788
app <- callr::r_bg(function(port) {
  tr <- rewindr::rewind_script(rewindr::rewind_example(), key = "order_id")
  shiny::runApp(rewindr::scrub_app(tr), port = port, launch.browser = FALSE)
}, args = list(port = port))

url <- sprintf("http://127.0.0.1:%d", port)
for (i in 1:60) {
  ok <- tryCatch({ readLines(url, warn = FALSE); TRUE }, error = function(e) FALSE)
  if (ok) break
  Sys.sleep(0.5)
}

out <- "vignettes/articles/figures"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

b <- chromote::ChromoteSession$new(width = 1440, height = 960)
loaded <- b$Page$loadEventFired(wait_ = FALSE)
b$Page$navigate(url, wait_ = FALSE)
b$wait_for(loaded)
Sys.sleep(5)

js <- function(code) b$Runtime$evaluate(code)
pick <- function(step) js(sprintf("Shiny.setInputValue('rw_pick', %d, {priority: 'event'})", step))
tab <- function(name) js(sprintf("document.querySelector('a[data-value=\"%s\"]').click()", name))
shot <- function(file) {
  Sys.sleep(1.5)
  b$screenshot(file.path(out, file), selector = "html", scale = 1, show = FALSE)
}

shot("app-overview.png")                       # opens on the first problem (step 8)
pick(9); tab("Join"); shot("app-join.png")
tab("What changed"); shot("app-diff.png")
pick(10); tab("Data"); shot("app-data.png")
pick(10); tab("Issues & fixes"); shot("app-issues.png")
tab("Ask Claude"); shot("app-claude.png")

b$close()
app$kill()
message("Screenshots written to ", normalizePath(out))
