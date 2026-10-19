# A realistic pipeline with three silent bugs. Run it with:
#   tr <- rewindr::rewind_script(system.file("examples", "broken_pipeline.R", package = "rewindr"))
library(dplyr)

set.seed(42)
n <- 1000
orders <- data.frame(
  order_id = 1:n,
  customer_id = sprintf("C%03d", sample(1:200, n, replace = TRUE)),
  amount = sprintf("%.2f", round(runif(n, 5, 500), 2)),
  order_date = format(as.Date("2026-01-01") + sample(0:180, n, replace = TRUE)),
  stringsAsFactors = FALSE
)
# Bug 1: an upstream export wrote some amounts with a currency symbol.
orders$amount[sample(n, 60)] <- paste0("$", orders$amount[sample(n, 60)])

customers <- data.frame(
  customer_id = sprintf("C%03d", 1:200),
  region = sample(c("North", "South", "East", "West"), 200, replace = TRUE),
  stringsAsFactors = FALSE
)
# Bug 2: the CRM pads a third of its ids with whitespace and lowercases some.
pad <- sample(200, 60)
customers$customer_id[pad] <- paste0(" ", tolower(customers$customer_id[pad]))
# Bug 3: a few customers were imported twice.
customers <- rbind(customers, customers[c(5, 17, 33), ])

clean <- orders |>
  mutate(amount = as.numeric(amount),
         order_date = as.Date(order_date)) |>
  inner_join(customers, by = "customer_id") |>
  filter(amount > 20)

monthly <- clean |>
  mutate(month = format(order_date, "%Y-%m")) |>
  group_by(region, month) |>
  summarise(revenue = sum(amount), orders = n(), .groups = "drop")
