test_that("rewind records each stage of a pipeline", {
  tr <- rewind(mtcars |> subset(cyl == 4) |> transform(kpl = mpg * 0.425))
  expect_s3_class(tr, "rewind_trace")
  tl <- timeline(tr)
  expect_equal(tl$rows, c(32, 11, 11))
  expect_equal(nrow(at_step(tr, "result")), 11)
  expect_true("kpl" %in% names(at_step(tr, 3)))
  expect_equal(step_diff(tr, 3)$cols_added, "kpl")
})

test_that("code runs in a sandbox and does not touch the caller's environment", {
  local({
    rewind({
      zz_should_not_leak <- data.frame(a = 1:3)
    })
    expect_false(exists("zz_should_not_leak", inherits = FALSE))
  })
})

test_that("errors are recorded instead of thrown", {
  tr <- rewind({
    a <- data.frame(x = 1:5)
    b <- a |> no_such_function()
  })
  expect_equal(tr$status, "error")
  last <- tr$steps[[length(tr$steps)]]
  expect_match(last$error, "no_such_function")
  expect_equal(nrow(at_step(tr, "a")), 5)
  expect_equal(first_divergence(tr), length(tr$steps))
})

test_that("in-place modifications create a new version with a diff", {
  tr <- rewind({
    d <- data.frame(x = c("1", "2", "oops"))
    d$x <- as.numeric(d$x)
  })
  expect_equal(length(lineage(tr, "d")), 2)
  d2 <- tr$steps[[tr$versions$d]]
  expect_equal(d2$diff$type_changes$to, "numeric")
  expect_true("new_na" %in% d2$issues$type)
  expect_true("r_warning" %in% d2$issues$type)
  b <- blame(tr, "d", "x")
  expect_length(b$findings, 2)
})

test_that("warnings are captured per step", {
  tr <- rewind(c("1", "a") |> as.numeric())
  expect_match(tr$steps[[2]]$warnings, "coercion")
})

test_that("scripts are traced with line numbers and object lineage", {
  code <- c(
    "raw <- data.frame(id = 1:10, v = c(1:9, NA))",
    "kept <- raw |>",
    "  subset(v > 2) |>",
    "  transform(w = v * 2)"
  )
  tr <- rewind_script(text = code)
  expect_equal(names(tr$versions), c("raw", "kept"))
  ids <- lineage(tr, "kept")
  expect_equal(tr$steps[[ids[1]]]$object, "raw")
  expect_equal(tr$steps[[ids[2]]]$line, 2)
  iss <- issues(tr)
  expect_true("filter_drops_na" %in% iss$type)
})

test_that("data.table snapshots are not mutated by reference", {
  skip_if_not_installed("data.table")
  tr <- rewind({
    library(data.table)
    dt <- data.table(a = 1:3)
    dt[, b := a * 2]
  }, env = globalenv())  # data.table only allows := from data.table-aware code
  versions <- Filter(function(s) identical(s$object, "dt"), tr$steps)
  expect_equal(length(versions), 2)
  expect_false("b" %in% names(versions[[1]]$value))
  expect_true("b" %in% names(versions[[2]]$value))
})
