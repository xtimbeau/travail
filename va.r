library(tidyverse)
library(eurostat)
library(ofce)

marchand <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "R", "S")
pays <- c("DE", "FR", "IT", "ES", "NL", "BE", "IE", "AT", "FI", "PT", "EL", "SK", "LU", "LT", "HR", "SI", "LV", "EE", "CY", "MT", "EA20")

na_nace_e <- get_eurostat("nama_10_a64_e",
                          filters = list(nace_r2  = marchand, geo = pays, unit = c("THS_HW"), na_item = c("SAL_DC", "SELF_DC")) ) |>
  drop_na(values) |>
  select(nace_r2, geo, time, values, na_item) |>
  pivot_wider(names_from = na_item, values_from = values)

na_nace <- get_eurostat("nama_10_a64", filters = list(unit = "CP_MEUR", nace_r2  = marchand, na_item = c("B1G", "D1", "P51C"), geo = pays) ) |>
  select(na_item, geo, time, values, nace_r2) |>
  drop_na(values) |>
  pivot_wider(id_cols = c(nace_r2, geo, time), names_from =  na_item, values_from = values ) |>
  left_join(na_nace_e, by = c("geo", "time", "nace_r2")) |>
  mutate(
    van = B1G-P51C,
    msa = D1 * (1+SELF_DC/SAL_DC) ) |>
  group_by(geo, time) |>
  summarize(van = sum(van),
            msa = sum(msa)) |>
  ungroup() |>
  mutate(psal = msa/van) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01")

ggplot(na_nace) +
  aes(x=time, y=psal, col = geo) +
  geom_line(data =~.x |>  rename(GEO = geo) , col = "gray") +
  geom_line() +
  facet_wrap(vars(geo)) +
  theme_ofce()
