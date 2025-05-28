library(tidyverse)
library(eurostat)
library(ofce)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")

marchand <- source_data("nace.r")

assets_a10 <- get_eurostat("nama_10_nfa_st",
                       filters = list(
                         asset10 = "N11N",
                         geo = pays2,
                         nace_r2 = marchand$marchand2, unit = "CRC_MEUR"  ) ) |>
  drop_na() |>
  pivot_wider(names_from = geo, values_from = values) |>
  arrange(desc(time)) |>
  select(time, nace_r2, BE, DE, ES, FR, IT, NL)

assets <- assets_a10 |>
  group_by(time) |>
  summarise(across(c(BE, DE, ES, FR, IT, NL), ~sum(.x, na.rm=TRUE))) |>
  arrange(desc(time))

return(list(assets = assets, assets_a10 = assets_a10))
