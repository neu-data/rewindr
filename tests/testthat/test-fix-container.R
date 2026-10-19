withr_tempdir <- function() {
  d <- tempfile("rewindr-")
  dir.create(d)
  d
}

test_that("ask_claude needs a key and builds a valid request", {
  skip_if_not_installed("httr2")
  tr <- rewind(mtcars |> subset(cyl == 4))
  expect_error(ask_claude(tr, api_key = ""), "ANTHROPIC_API_KEY")
  ctx <- rewindr:::trace_context(tr)
  expect_match(ctx, "## Source code")
  expect_match(ctx, "## Recorded timeline")
  schema_json <- jsonlite::toJSON(rewindr:::.claude_schema, auto_unbox = TRUE)
  expect_match(schema_json, '"required":\\["diagnosis","changes","code"\\]')
  expect_match(schema_json, '"additionalProperties":false')
})

test_that("script_packages finds library() and pkg:: references", {
  pk <- script_packages(text = c("library(stats)", "library(utils)", "x <- tools::file_ext('a.R')",
                                 "requireNamespace('jsonlite')", "y <- df[, 1]"))
  expect_setequal(pk$package, c("jsonlite"))
  expect_true(all(pk$installed))
})

test_that("containerize writes a pinned Dockerfile and copies data", {
  dir <- withr_tempdir()
  writeLines("a,b\n1,2", file.path(dir, "input.csv"))
  writeLines(c("library(jsonlite)", "d <- read.csv('input.csv')", "print(toJSON(d))"), file.path(dir, "job.R"))
  suppressMessages(containerize(file.path(dir, "job.R")))
  df <- readLines(file.path(dir, "container", "Dockerfile"))
  expect_true(any(grepl("^FROM rocker/r-ver:", df)))
  expect_true(any(grepl(paste0("jsonlite@", utils::packageVersion("jsonlite")), df, fixed = TRUE)))
  expect_true(any(grepl("input.csv", df, fixed = TRUE)))
  expect_true(file.exists(file.path(dir, "container", "input.csv")))
  expect_error(containerize(file.path(dir, "job.R")), "exists")
})
