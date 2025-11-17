library(tidyverse)
library(ofce)
library(ggflags)
library(ggiraph)
library(countrycode)
library(glue)


naq_full <- ofce::source_data("vaq.r")$naa |>
  filter(champ == "mdhi")

data <- naq_full |>
  mutate(year = year(time)) |>
  filter( year %in% c(1995, 2025)) |>
  select(year, geo, psal, van, psalb, vab) |>
  pivot_wider(names_from = year, values_from = c(psal,van, psalb, vab)) |>
  mutate(geo = fct_reorder(geo, psal_2025)) |>
  mutate(geo2c = countrycode(geo, "eurostat", "iso2c"))

data_int <- data |>
  full_join(naq_full |> select(time, psal, geo), by = "geo") |>
  group_by(geo) |>
  mutate(
    year = year(time),
    i = (year - 1995)/(2025 - 1995),
    psal_seg = psal_2025 * i  + psal_1995 * (1-i),
    x  = geo, xend = geo,
    y = lag(psal_seg),
    yend = psal_seg ) |>
  mutate(
    tooltip = glue("<b>{lbl(geo)}</b>
                     Part des salaires dans la VAN en {year} : {round(100*psal,1)}%
                     (VA nette, cor. non sal., marchandes hors imm.)")) |>
  ungroup()


data_intb <- data |>
  mutate(geo = fct_reorder(geo, psalb_2025)) |>
  full_join(naq_full |> select(time, psalb, geo), by = "geo") |>
  group_by(geo) |>
  mutate(
    year = year(time),
    i = (year - 1995)/(2025 - 1995),
    psal_seg = psalb_2025 * i  + psalb_1995 * (1-i),
    x  = geo, xend = geo,
    y = lag(psal_seg),
    yend = psal_seg ) |>
  mutate(
    tooltip = glue("<b>{lbl(geo)}</b>
                     Part des salaires dans la VAB en {year} : {round(100*psalb,1)}%
                     (VA brute, cor. non sal., marchandes hors imm.)")) |>
  ungroup()


return(list(data = data, int = data_int, intb = data_intb))
