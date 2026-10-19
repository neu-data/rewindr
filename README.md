
<!-- README.md is generated from README.Rmd. Please edit that file and rebuild with devtools::build_readme() -->

<div align="center">
<img src="https://raw.githubusercontent.com/neu-data/.github/main/assets/banner.svg" alt="Neudata Consulting Ltd" width="100%" />
</div>

# rewindr <img src="man/figures/logo.png" align="right" height="139" alt="rewindr logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/neu-data/rewindr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/neu-data/rewindr/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-055F56?style=flat)](https://neu-data.github.io/rewindr/)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-0B376C.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![Neudata](https://img.shields.io/badge/Neudata-R%20package-04242F?style=flat)
<!-- badges: end -->

**Time-travel debugging for data pipelines.**

Your monthly report is a third short. Somewhere upstream a join dropped
customers whose ids had a trailing space, a conversion turned `"$12.50"`
into `NA`, and a filter quietly removed those rows. Finding that usually
means adding `print()` calls and running the script again, several
times.

rewindr records the data after **every** step of a pipeline or script,
compares each step with the one before, and tells you the first step
where the data went wrong, and why. Then you can scrub backward and
forward through every state in an interactive app.

<figure>
<img src="man/figures/README-app.png"
alt="The rewindr time-travel app" />
<figcaption aria-hidden="true">The rewindr time-travel app</figcaption>
</figure>

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("neu-data/rewindr")
```

For the app and the Claude integration, also install the suggested
packages:

``` r
install.packages(c("shiny", "bslib", "DT", "httr2", "jsonlite"))
```

## Example

The package ships a small sales pipeline with three bugs planted in it.
Record it, with `order_id` as the row key:

``` r
library(rewindr)
tr <- rewind_script(rewind_example(), key = "order_id")
tr
#> <rewind_trace> 13 steps recorded
#>      1  n <- 1000                                                         numeric         
#>      2  orders <- data.frame( ...                                       1,000 x 4         
#>      3  orders$amount[sample(n, 60)] <- paste0("$", orders$amou...      1,000 x 4         
#>      4  customers <- data.frame( ...                                      200 x 2         
#>      5  pad <- sample(200, 60)                                            integer         
#>      6  customers$customer_id[pad] <- paste0(" ", tolower(custo...        200 x 2         
#>      7  customers <- rbind(., customers[c(5, 17, 33), ])                  203 x 2       +3
#>  !   8  mutate(., amount = as.numeric(amount), order_date = as....      1,000 x 4         
#>  !   9  inner_join(., customers, by = "customer_id")                      730 x 5     -270
#>  !  10  clean <- filter(., amount > 20)                                   663 x 5      -67
#>     11  mutate(., month = format(order_date, "%Y-%m"))                    663 x 6         
#>     12  group_by(., region, month)                                        663 x 6         
#>     13  monthly <- summarise(., revenue = sum(amount), orders =...         24 x 4     -639
#> 
#> First divergence at step 8: R warning: There was 1 warning in `mutate()`. In argument: `amount = as.numeric(amount)`. NAs introduced by coercion
#>   fix: Values could not be parsed; inspect them before converting (e.g. readr::parse_number or a regex clean-up).
#> Run scrub(tr) to explore, or issues(tr) for the full list.
```

Ask why `clean` looks the way it does:

``` r
blame(tr, "clean")
#> Lineage of clean: 5 steps
#> 
#> > step 8 (line 28): mutate(., amount = as.numeric(amount), order_date = as.Date(order_date))
#>     R warning: There was 1 warning in `mutate()`. In argument: `amount = as.numeric(amount)`. NAs introduced by coercion
#>     fix: Values could not be parsed; inspect them before converting (e.g. readr::parse_number or a regex clean-up).
#>     Column 'amount' gained 60 missing values (now 6.0% NA).
#>     fix: The type conversion could not parse some values.
#> 
#> > step 9 (line 28): inner_join(., customers, by = "customer_id")
#>     R warning: Detected an unexpected many-to-many relationship between `x` and `y`. Row 26 of `x` matches multiple rows in `y`. Row 49 of `y` matches multiple rows in `x`. If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
#>     281 of 1,000 left-hand rows (28.1%) were dropped in inner_join on [customer_id].
#>     fix: 59 unmatched keys match after trimming whitespace / ignoring case (e.g. 'C065'). Normalise the keys with trimws()/toupper() before the join.
#>     Right-hand table has 3 duplicated key(s) (e.g. 'C005', 'C017', ' c033'); 11 left rows were multiplied.
#>     fix: De-duplicate the lookup table on its key (distinct(y, key, .keep_all = TRUE)) or add the missing key columns to `by`.
#>     Key [order_id] is no longer unique.
#> 
#> > step 10 (line 28): clean <- filter(., amount > 20)
#>     42 of the removed rows were dropped only because [amount] is NA, not because of the condition.
#>     fix: Missing values silently fail the filter condition. Fix them upstream, or keep them explicitly (e.g. `| is.na(col)`).
```

Then open the app:

``` r
scrub(tr)
```

In RStudio, **Addins â†’ Rewind current script** does all of this for
the script in your editor.

## What it does

|  |  |
|----|----|
| **Records every state** | Splits `|>` and `%>%` pipelines into stages, runs scripts statement by statement, and turns in-place edits (`df$x <- ...`, `dt[, x := ...]`) into new versions. Code runs in a sandbox, and errors are recorded instead of thrown. |
| **Compares states** | Rows gained or lost, columns added or removed, type changes, new `NA`s and changed cells, aligned by position or by `key`. |
| **Checks joins** | Unmatched rows, keys that differ only in whitespace or case, duplicated lookup keys that multiply rows, and key type mismatches. |
| **Finds the first divergence** | Silent row loss, join fan-out, coercion `NA`s, filters that drop rows only because of `NA`, and empty results, tuned with `rewind_rules()`. `blame()` follows an object or column back to the step that explains it. |
| **Scrubs through time** | `scrub()` opens an app with a slider, play button and arrow keys, the highlighted source line, the data at each step, the diff and the join report. |
| **Proposes fixes** | `suggest_fixes()` works offline. `ask_claude()` returns a diagnosis and a revised script, which you can re-run in a sandbox and compare with `compare_traces()`. |
| **Ships the script** | `containerize()` writes a Dockerfile with your R version and every package pinned at the installed version. |

## Learn more

- [Get
  started](https://neu-data.github.io/rewindr/articles/rewindr.html)
- [Using rewindr every
  day](https://neu-data.github.io/rewindr/articles/daily-workflow.html)
- [A tour of the time-travel
  app](https://neu-data.github.io/rewindr/articles/time-travel-app.html)
- [What rewindr
  checks](https://neu-data.github.io/rewindr/articles/what-rewindr-checks.html)
- [Fixing code and shipping
  it](https://neu-data.github.io/rewindr/articles/fixing-and-shipping.html)

------------------------------------------------------------------------

Developed by [Neudata Consulting Ltd](https://www.neu-data.com) Â·
*Insight. Impact. Innovation.*
