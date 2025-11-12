library(tidyverse)
library(ofce)
library(ggimage)

date <- sourcoise::sourcoise_meta("vaq.r")$data_date |> ymd_hms()
date <- stringr::str_glue("{day(date)}/{month(date)}/{year(date)}")

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

psaleu <- ggplot(data_int) +
  geom_segment(
    aes(x = x,  xend = xend, y = y, yend = yend, color = year),
    key_glyph = draw_key_point, linewidth = 0.2) +
  ggimage::geom_flag( data = ~.x |> filter(i==1),
                      aes(x= geo, y = 0.3, image = geo2c), size = .03)+
  scale_color_gradient(low="seagreen1", high = "seagreen4",
                       name = NULL,
                       breaks = c(1995,2025), labels = c(1995, 2025)) +
  geom_point_interactive(
    data = ~.x |> filter(!i%in%c(0,1)),
    hover_nearest = TRUE,
    aes(x = geo, y=psal, tooltip = tooltip, color = year),
    size = 0.3, alpha = 0.5)+
  geom_point_interactive(
    data = ~.x |> filter(i%in%c(0,1)),
    aes(x = geo, y=yend, size = van_2025, color = year,
        tooltip = tooltip),
    hover_nearest = TRUE)  +
  coord_flip()+
  guides(color = guide_legend(), size= "none") +
  scale_x_discrete(labels = ~str_c(lbl(.x))) +
  scale_y_continuous(labels = scales::label_percent(1),
                     limits = c(0.325,1), oob = squish) +
  scale_size(range = c(1,3)) +
  labs(x=NULL, y=NULL)+
  theme(
    legend.direction = "horizontal",
    panel.grid.major.y = element_blank(),
    legend.text = element_marquee(size = rel(0.7)),
    axis.text.y = element_marquee(size = rel(1),
                                  vjust = 0.5, hjust = 1,
                                  margin = margin(r=-10, t=0, b=2)),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  ofce_caption(
    source = "Eurostat, comptes nationaux annuels (nama_10_a64, nama_10_a64_e),
 comptes nationaux trimestriels (namq_10_a10), téléchargés le {date}, code à github.com/xtimbeau/travail/R/vaq.r.",
    champ = "Branches marchandes hors services immobiliers (-L).",
    note = "Part des salaires dans la valeur ajoutée nette corrigée de la non salarisation.")

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

psalbeu <- ggplot(data_intb) +
  geom_segment(
    aes(x = x,  xend = xend, y = y, yend = yend, color = year),
    key_glyph = draw_key_point, linewidth = 0.2) +
  ggimage::geom_flag( data = ~.x |> filter(i==1),
                      aes(x= geo, y = 0.2, image = geo2c), size = .03)+
  scale_color_gradient(low="orange1", high = "darkorange4",
                       name = NULL,
                       breaks = c(1995,2025), labels = c(1995, 2025)) +
  geom_point_interactive(
    data = ~.x |> filter(!i%in%c(0,1)),
    hover_nearest = TRUE,
    aes(x = geo, y=psalb, tooltip = tooltip, color = year),
    size = 0.3, alpha = 0.5)+
  geom_point_interactive(
    data = ~.x |> filter(i%in%c(0,1)),
    aes(x = geo, y=yend, size = vab_2025, color = year,
        tooltip = tooltip),
    hover_nearest = TRUE)  +
  coord_flip()+
  guides(color = guide_legend(), size= "none") +
  scale_x_discrete(labels = ~str_c("**", lbl(.x), "**")) +
  scale_y_continuous(labels = scales::label_percent(1),
                     limits = c(0.25,1), oob = squish) +
  scale_size(range = c(1,3)) +
  labs(x=NULL, y=NULL)+
  theme(
    legend.direction = "horizontal",
    panel.grid.major.y = element_blank(),
    legend.text = element_marquee(size = rel(0.7)),
    axis.text.y = element_marquee(size = rel(1),
                                  vjust = 0.5, hjust = 1,
                                  margin = margin(r=-10, t=0, b=2)),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  ofce_caption(
    source = "Eurostat, comptes nationaux annuels (nama_10_a64, nama_10_a64_e),
 comptes nationaux trimestriels (namq_10_a10), téléchargés le {date}, code à github.com/xtimbeau/travail/vaq.r.",
    champ = "Branches marchandes hors services immobiliers (-L).",
    note = "Part des salaires dans la valeur ajoutée brute corrigée de la non salarisation.")

return(list(gg = list("VA nette" = psaleu, "VA brute" = psalbeu), data = data))
