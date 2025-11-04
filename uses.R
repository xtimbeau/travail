library(tidyverse)
library(eurostat)
library(ofce)
library(melodi)

nace <- source_data("nace.r")$nace
a20 <- nace$a20
pays <- source_data("nace.r")$pays1

vahi <- source_data("vaq.r")$naa |>
  filter(champ %in% c("mdhi"), geo %in% P6) |>
  select(geo, time, van, vab)

uses_15 <- "naio_10_cp15" |>
  eurostat::get_eurostat(
    filters = list(
      geo = pays,
      ind_impv = a20,
      prd_amo = c("CPA_L68A", "CPA_L68B"),
      unit = "MIO_EUR") ) |>
  filter(stk_flow == "TOTAL", prd_amo == "CPA_L68B") |>
  rename(CI_L = values) |>
  drop_na(CI_L) |>
  left_join(nace |> select(a20, md = marchand, hi=hors_imm), by = c("ind_impv"="a20")) |>
  group_by(geo, time) |>
  summarize(
    across(c(CI_L), ~sum(.x[md&hi]), .names = "{.col}"),
    .groups = "drop") |>
  select(geo, time, CI_L) |>
  left_join(vahi, by = c("geo", "time")) |>
  arrange(geo, desc(time))

uses_1610 <- "naio_10_cp1610" |>
  eurostat::get_eurostat(
    filters = list(
      geo = pays,
      ind_use = a20,
      prd_ava = c("CPA_L68A", "CPA_L68B"),
      unit = "MIO_EUR") ) |>
  filter(stk_flow == "TOTAL", prd_ava == "CPA_L68B") |>
  rename(CI_L = values) |>
  drop_na() |>
  left_join(nace |> select(a20, md = marchand, hi=hors_imm), by = c("ind_use"="a20")) |>
  group_by(geo, time) |>
  summarize(
    across(c(CI_L), ~sum(.x[md&hi]), .names = "{.col}"),
    .groups = "drop") |>
  select(geo, time, CI_L) |>
  left_join(vahi, by = c("geo", "time")) |>
  arrange(geo, desc(time))

return(list(u15 = uses_15, u1610 = uses_1610))

# left_join(vahim, uses, by = c("geo", "time")) |>
#   mutate(r2 = CI_L/mdhi) |>
#   drop_na() |>
#   ggplot()+
#   aes(x=time, y=r2, color = geo, group = geo)+
#   geom_line(layout = "fixed", color = "gray85") +
#   geom_line() +
#   geom_line(aes(y=r), linetype = "11") +
#   facet_wrap(vars(geo)) +
#   scale_color_pays("eurostat") +
#   theme_ofce()
