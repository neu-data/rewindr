# Opens the time-travel UI on the bundled broken pipeline.
#   Rscript -e "source(system.file('examples', 'run_demo_app.R', package = 'rewindr'))"
library(rewindr)
tr <- rewind_script(system.file("examples", "broken_pipeline.R", package = "rewindr"),
                    key = "order_id")
print(tr)
port <- as.integer(Sys.getenv("REWINDR_PORT", "7744"))
scrub(tr, launch.browser = interactive(), port = port)
