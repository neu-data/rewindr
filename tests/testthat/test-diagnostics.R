demo <- function() system.file("examples", "broken_pipeline.R", package = "rewindr")

test_that("df_diff reports rows, columns, types and NAs", {
  a <- data.frame(id = 1:4, x = c("1", "2", "3", "x"), stringsAsFactors = FALSE)
  b <- transform(a, x = suppressWarnings(as.numeric(x)), y = 1)
  d <- df_diff(a, b)
  expect_equal(d$cols_added, "y")
  expect_equal(d$type_changes$column, "x")
  expect_equal(d$na_delta$new_na[d$na_delta$column == "x"], 1)
  expect_equal(d$aligned_by, "position")
})

test_that("df_diff aligns rows by key", {
  a <- data.frame(id = 1:5, v = 1:5)
  b <- data.frame(id = c(2, 3, 4, 6), v = c(2, 30, 4, 6))
  d <- df_diff(a, b, key = "id")
  expect_setequal(d$dropped_keys, c("1", "5"))
  expect_equal(d$added_keys, "6")
  expect_equal(unname(d$changed_cells[["v"]]), 1)
})

test_that("join diagnostics find near-miss keys and duplicated lookup keys", {
  x <- data.frame(k = c("A1", "A2", "A3", "A4"), v = 1:4)
  y <- data.frame(k = c("A1", " a2", "A3", "A3"), w = 1:4)
  j <- rewindr:::join_report(x, y, list(x = "k", y = "k"))
  expect_equal(j$unmatched_rows, 2)
  expect_equal(j$near_miss_keys, "A2")
  expect_equal(j$dup_y_keys, 1)
  expect_equal(j$fanout_rows, 1)
})

test_that("the demo pipeline's three planted bugs are found", {
  skip_if_not_installed("dplyr")
  tr <- rewind_script(demo())
  iss <- issues(tr)
  expect_true(all(c("new_na", "join_unmatched", "join_fanout", "filter_drops_na") %in% iss$type))
  join_issue <- iss[iss$type == "join_unmatched", ]
  expect_match(join_issue$hint, "whitespace")
  expect_equal(first_divergence(tr), min(iss$step))
  fx <- suggest_fixes(tr)
  expect_true(any(grepl("trimws", fx$suggestion)))
  expect_true(any(grepl("distinct", fx$suggestion)))
  expect_true(any(grepl("as.numeric\\(gsub", fx$suggestion)))
})

test_that("aggregation steps are not reported as row loss", {
  skip_if_not_installed("dplyr")
  tr <- rewind(mtcars |> dplyr::group_by(cyl) |> dplyr::summarise(n = dplyr::n()))
  expect_equal(nrow(issues(tr)), 0)
})

test_that("a fixed script can be re-run and compared", {
  skip_if_not_installed("dplyr")
  old <- rewind_script(demo())
  code <- readLines(demo())
  code <- sub("inner_join(customers, by = \"customer_id\")",
              "inner_join(customers |> mutate(customer_id = toupper(trimws(customer_id))) |> distinct(customer_id, .keep_all = TRUE), by = \"customer_id\")",
              code, fixed = TRUE)
  code <- sub("amount = as.numeric(amount)", "amount = as.numeric(gsub(\"[^0-9.]\", \"\", amount))", code, fixed = TRUE)
  new <- rewind_script(text = code)
  cmp <- compare_traces(old, new)
  expect_true(attr(cmp, "issues")[["new"]] < attr(cmp, "issues")[["old"]])
  expect_equal(cmp$rows_new[cmp$object == "orders"], 1000)
  expect_equal(nrow(issues(new)), 0)
})
