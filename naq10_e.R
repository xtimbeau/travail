library(tidyverse)
library(eurostat)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
nace <- source_data("nace.r")
adj <- c("SCA", "SA", "CA", "NSA")

naq <- get_eurostat("namq_10_a10",
                    filters = list(
                      nace_r2 = nace$marchand2,
                      na_item = "D1",
                      geo = pays2,
                      unit = "CP_MEUR") ) |>
  drop_na() |>
  mutate(s_adj = factor(s_adj, adj)) |>
  group_by(nace_r2, geo, time) |>
  summarise(across(c(values, s_adj), first),
            .groups = "drop") |>
  select(geo, time, nace_r2, values, s_adj) |>
  rename(D1 = values)

naa_e.raw <- get_eurostat("nama_10_a64_e",
                          filters = list(nace_r2  = nace$marchand, geo = pays2,
                                         unit = c("THS_PER"),
                                         na_item = c("SAL_DC", "SELF_DC")) ) |>
  drop_na(values) |>
  select(nace_r2, geo, time, values, na_item) |>
  pivot_wider(names_from = na_item, values_from = values) |>
  left_join(nace$nace |> select(a20, a10), by = c("nace_r2"="a20")) |>
  group_by(a10, geo, time) |>
  summarize(across(c(SAL_DC, SELF_DC), sum),
            .groups = "drop") |>
  rename(nace_r2 = a10)

naa_e <- naa_e.raw |>
  transmute(
    geo, time, nace_r2, SAL_DC, SELF_DC) |>
  cross_join(tibble(month = seq(1, 10, by=3))) |>
  mutate(time = str_c(year(time), " ", month) |> lubridate::ym())

naq_e <- naq |>
  left_join(naa_e, by = c("geo", "time", "nace_r2")) |>
  group_by(geo, nace_r2) |>
  arrange(time) |>
  fill(SAL_DC, SELF_DC) |>
  group_by(geo, time, nace_r2) |>
  summarize( salw = sum(D1, na.rm = TRUE),
             selfw = sum(D1/SAL_DC*SELF_DC, na.rm = TRUE),
             sal = sum(SAL_DC, na.rm = TRUE),
             self = sum(SELF_DC, na.rm=TRUE),
             .groups = "drop" ) |>
  transmute( time, geo, nace_r2,
             tsalw = 1+selfw/salw,
             tsal = 1+self/sal ) |>
  complete(time, geo, nace_r2) |>
  group_by(geo, nace_r2) |>
  fill(tsal, tsalw) |>
  ungroup()

return(list(naq_e = naq_e, naa_e = naa_e))
