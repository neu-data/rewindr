test_that("native pipes are split into stages", {
  p <- rewindr:::unwind_pipeline(quote(mtcars |> subset(cyl == 4) |> head(3)))
  expect_identical(p$base, quote(mtcars))
  expect_length(p$stages, 2)
  expect_equal(vapply(p$stages, `[[`, "", "fun"), c("subset", "head"))
  expect_equal(rewindr:::stage_code(p$stages[[1]]), "subset(., cyl == 4)")
})

test_that("magrittr pipes keep their semantics", {
  skip_if_not_installed("magrittr")
  library(magrittr)
  p <- rewindr:::unwind_pipeline(quote(mtcars %>% subset(cyl == 4) %>% nrow()))
  expect_length(p$stages, 2)
  expect_equal(p$stages[[1]]$mode, "magrittr")
  tr <- rewind(mtcars %>% subset(cyl == 4) %>% lm(mpg ~ wt, data = .))
  expect_s3_class(at_step(tr, 3), "lm")
})

test_that("operators and calls with literal first arguments are not split", {
  expect_length(rewindr:::unwind_pipeline(quote(a + b))$stages, 0)
  expect_length(rewindr:::unwind_pipeline(quote(read.csv("x.csv")))$stages, 0)
  expect_length(rewindr:::unwind_pipeline(quote(data.frame(x = 1)))$stages, 0)
})

test_that("empty arguments are handled", {
  expect_length(rewindr:::call_parts(quote(x[, 1])), 3)
})
