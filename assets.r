library(tidyverse)
library(eurostat)
library(ofce)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")

nace <- source_data("nace.r")$nace
m_a20 <- nace  |> pull(a20) |> unique()
m_a10 <- nace |> pull(a10) |> unique()

assets_a10 <- get_eurostat("nama_10_nfa_st",
                           filters = list(
                             asset10 = "N11N",
                             geo = pays2,
                             nace_r2 = m_a20, unit = "CRC_MEUR"  ) ) |>
  drop_na() |>
  arrange(desc(time)) |>
  select(time, geo, nace_r2, assets = values)

men <- "nama_10_nfa_bs" |>
  get_eurostat(
    filters = list(
      asset10 = "N111N",
      geo = pays2,
      sector = "S14_S15", unit = "CP_MEUR" ) ) |>
  transmute(geo, time, wim = values) |>
  drop_na(wim)

assets <- assets_a10 |>
  left_join(nace |> select(a20, hifi, md=marchand, hi = hors_imm, hfi), by = c("nace_r2"= "a20")) |>
  group_by(time, geo) |>
  summarise(
    assets_md = sum(assets[md], na.rm=TRUE),
    assets_mdhi = sum(assets[md&hi], na.rm=TRUE),
    assets_mdhifi = sum(assets[md&hifi], na.rm=TRUE),
    assets_mdhfi = sum(assets[md&hfi], na.rm=TRUE),
    assets_tb = sum(assets, na.rm=TRUE),
    .groups = "drop") |>
  left_join(men, by = c("geo", "time")) |>
  mutate(assets_mdhim = assets_md - wim) |>
  select(-wim) |>
  pivot_longer(starts_with("assets_")) |>
  separate(name, into = c("var", "champ"), sep = "_") |>
  pivot_wider(names_from = var, values_from = value) |>
  arrange(desc(time))

return(list(assets = assets, assets_a10 = assets_a10))
