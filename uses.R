library(tidyverse)
library(eurostat)
library(ofce)

pays <- source_data("nace.r")$pays1

vahim <- source_data("vaq.r")$naa |>
  select(geo, time, van, champ) |>
  filter(champ %in% c("mdhim", "mdhi"), geo %in% P6) |>
  pivot_wider(names_from = champ, values_from = van) |>
  mutate(r = (mdhim-mdhi)/mdhi)

ggplot(vahim) +
  aes(x=time, y=r, color = geo, group = geo)+
  geom_line(layout = "fixed", color = "gray85") +
  geom_line() +
  facet_wrap(vars(geo)) +
  scale_color_pays() +
  theme_ofce()


uses <- "naio_10_cp1610" |>
  eurostat::get_eurostat(
    filters = list(
      geo = pays,
      ind_use = "TOTAL",
      prd_ava = c("CPA_L68A", "CPA_L68B"),
      unit = "MIO_EUR") ) |>
  filter(stk_flow == "TOTAL", prd_ava == "CPA_L68B") |>
  rename(CI_L = values) |>
  drop_na() |>
  select(geo, time, CI_L)

left_join(vahim, uses, by = c("geo", "time"))
