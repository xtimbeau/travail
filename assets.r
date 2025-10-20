library(tidyverse)
library(eurostat)
library(ofce)

pays <- source_data("nace.r")$pays3

nace <- source_data("nace.r")$nace
m_a20 <- nace  |> pull(a20) |> unique()
m_a10 <- nace |> pull(a10) |> unique()

assets_a10 <- get_eurostat("nama_10_nfa_st",
                           filters = list(
                             asset10 = c("N11N", "N111N"),
                             geo = pays,
                             nace_r2 = m_a20, unit = "CRC_MEUR"  ) ) |>
  drop_na() |>
  arrange(desc(time)) |>
  select(time, geo, nace_r2, asset10, values) |>
  pivot_wider(names_from = asset10, values_from = values) |>
  mutate(N111N = replace_na(N111N, 0))


men <- "nama_10_nfa_bs" |>
  get_eurostat(
    filters = list(
      asset10 = "N111N",
      geo = pays,
      sector = "S14_S15", unit = "CP_MEUR" ) ) |>
  transmute(geo, time, wim = values) |>
  drop_na(wim)

assets <- assets_a10 |>
  left_join(nace |> select(a20, hifi, md=marchand, hi = hors_imm, hfi), by = c("nace_r2"= "a20")) |>
  complete(geo, time, nace_r2) |>
  group_by(time, geo) |>
  summarise(
    assets_md = sum(assets[md]),
    assets_mdhi = sum(assets[md&hi]),
    assets_mdhifi = sum(assets[md&hifi]),
    assets_mdhfi = sum(assets[md&hfi]),
    assets_tb = sum(assets),
    .groups = "drop") |>
  left_join(men, by = c("geo", "time")) |>
  mutate(assets_mdhim = assets_md - wim,
         assets_mdhimfi = assets_mdhfi - wim) |>
  select(-wim) |>
  pivot_longer(starts_with("assets_")) |>
  separate(name, into = c("var", "champ"), sep = "_") |>
  pivot_wider(names_from = var, values_from = value) |>
  drop_na(assets) |>
  mutate(geo = factor(geo, pays)) |>
  arrange(geo, desc(time))

assets_dom <- assets_a10 |>
  group_by(geo) |>
  summarize(deb = year(min(time)), fin = max(year(time)))


return(list(assets = assets, assets_a10 = assets_a10, dom = assets_dom))
