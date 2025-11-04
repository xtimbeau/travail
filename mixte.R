library(tidyverse)
library(eurostat)
library(ofce)

nace <- source_data("nace.r")
pays <- nace$pays3

adj <- c("SCA", "SA", "CA", "NSA")

mixte_q <- "nasq_10_nf_tr" |>
  get_eurostat(
    filters = list(
      unit = "CP_MEUR",
      direct = "PAID",
      na_item = "B3G",
      geo = pays)) |>
  filter(sector == "S14_S15") |>
  mutate(s_adj = factor(s_adj, adj)) |>
  drop_na() |>
  group_by(geo, time) |>
  arrange(s_adj, geo, time) |>
  summarise(b3g = first(values)) |>
  transmute(geo, time, b3g)

mixte_a <- "nasa_10_nf_tr" |>
  get_eurostat(
    filters = list(
      unit = "CP_MEUR",
      direct = "PAID",
      na_item = "B3G",
      geo = pays)) |>
  filter(sector == "S14_S15") |>
  transmute(geo, time, b3g=values) |>
  drop_na()

return(list(q = mixte_q, a = mixte_a))
