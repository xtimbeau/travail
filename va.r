library(tidyverse)
library(eurostat)
library(ofce)

nace <- tribble( ~a20, ~a10, ~marchand,
                     "A", "A", TRUE,
                     "B", "B-E", TRUE,
                     "C", "B-E", TRUE,
                     "D", "B-E", TRUE,
                     "E", "B-E", TRUE,
                     "F", "F", TRUE,
                     "G", "G-I", TRUE,
                     "H", "G-I", TRUE,
                     "I", "G-I", TRUE,
                     "J", "J", TRUE,
                     "K", "K", TRUE,
                     "L", "L", TRUE,
                     "M", "M_N", TRUE,
                     "N", "M_N", TRUE,
                     "O", "O-Q", FALSE,
                     "P", "O-Q", FALSE,
                     "Q", "O-Q", FALSE,
                     "R", "R-U", TRUE,
                     "S", "R-U", TRUE,
                     "T", "R-U", TRUE,
                     "U", "R-U", TRUE ) |>
  mutate(l20 = label_eurostat(a20, dic="nace_r2"),
         l10 = label_eurostat(a10, dic="nace_r2"))

marchand <- nace |> filter(marchand) |> pull(a20)
marchand2 <- c(nace |> filter(marchand) |> pull(a10))
pays <- c("DE", "FR", "IT", "ES", "NL", "BE", "IE", "AT", "FI", "PT", "EL", "SK", "LU", "LT", "HR", "SI", "LV", "EE", "CY", "MT", "EA20")

naa_e <- get_eurostat("nama_10_a64_e",
                          filters = list(nace_r2  = marchand, geo = pays, unit = c("THS_HW"), na_item = c("SAL_DC", "SELF_DC")) ) |>
  drop_na(values) |>
  select(nace_r2, geo, time, values, na_item) |>
  pivot_wider(names_from = na_item, values_from = values)

naa_a20 <- get_eurostat("nama_10_a64", filters = list(unit = "CP_MEUR", nace_r2  = marchand, na_item = c("B1G", "D1", "P51C"), geo = pays) ) |>
  select(na_item, geo, time, values, nace_r2) |>
  drop_na(values) |>
  pivot_wider(id_cols = c(nace_r2, geo, time), names_from =  na_item, values_from = values ) |>
  left_join(naa_e, by = c("geo", "time", "nace_r2")) |>
  left_join(nace |> select(nace_r2 = a20, a10), by = "nace_r2" ) |>
  mutate(
    van = B1G-P51C,
    msa = D1 * (1+SELF_DC/SAL_DC) )

naa_a10 <- naa_a20 |>
  group_by(geo, time, a10) |>
  summarize(
    van = sum(van, na.rm = TRUE),
    msa = sum(msa, na.rm = TRUE),
    .groups = "drop")

naa <- naa_a20 |>
  group_by(geo, time) |>
  summarize(van = sum(van, na.rm=TRUE),
            msa = sum(msa, na.rm=TRUE)) |>
  ungroup() |>
  mutate(psal = msa/van) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01")

max_y <- max(year(naa_10$time))

naq <- get_eurostat("namq_10_a10", filters = list(unit = "CP_MEUR",  na_item = c("B1G", "D1"), nace_r2  = marchand2, geo = pays)) |>
  select(geo, time, nace_r2, na_item, values, s_adj) |>
  drop_na(values) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  arrange(s_adj) |>
  group_by(geo, time, nace_r2, na_item) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  group_by(geo, time, nace_r2, na_item) |>
  filter(time >= yq(str_c(max_y, "-1"))) |>
  arrange(time) |>
  group_by(nace_r2, geo, na_item) |>
  mutate(
    acquis = lag(values)/slider::slide_dbl(lag(values), .before = 3, .f = sum),
    qch = values/lag(values)-1) |>
  ungroup() |>
  filter(year(time) > max_y) |>
  mutate(q  = quarter(time)) |>
  complete(geo, na_item, nace_r2, q = 1:4) |>
  group_by(na_item, nace_r2, geo) |>
  mutate(qch = replace_na(qch, mean(qch, na.rm=TRUE)),
         acquis = replace_na(acquis, mean(acquis, na.rm=TRUE)),
         value_q = cumprod(1+qch)) |>
  mutate(values_a = acquis * sum(value_q)) |>
  ungroup() |>
  filter(q == 4) |>
  select(geo, na_item, nace_r2, values_a) |>
  mutate(time = ym(str_c(max_y+1, "-01")),
         na_item = case_match(na_item,
                             "B1G" ~ "van",
                             "D1" ~ "msa") ) |>
  pivot_wider(names_from = na_item, values_from = values_a)

na_tot <- get_eurostat("nama_10_gdp", filters = list(geo = pays, na_item = c("D21", "D31"), unit = "CP_MEUR")) |>
  drop_na() |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time, d2131 = D21 - D31)

nq_tot <- get_eurostat("namq_10_gdp", filters = list(geo = pays, na_item = c("D21X31"), unit = "CP_MEUR")) |>
  drop_na() |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  arrange(s_adj) |>
  group_by(geo, time, na_item) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  filter(year(time)>max_y) |>
  group_by(geo, na_item) |>
  summarize(values = mean(values, na.rm=TRUE) * 4) |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time = ym(str_c(max_y+1, "-01")), d2131 = D21X31) |>
  bind_rows(na_tot) |>
  arrange(geo, time)

naa_ext <- naa_a10 |>
  select(nace_r2 = a10, geo, time, van, msa) |>
  bind_rows(naq |> rename(vana=van, msaa = msa)) |>
  arrange(time, geo, nace_r2) |>
  group_by(geo, nace_r2) |>
  mutate(
    vana = lag(van)*vana,
    msaa = lag(msa)*msaa,
    van = ifelse(is.na(van), vana, van),
    msa = ifelse(is.na(msa), msaa, msa)) |>
  select(-msaa, -vana) |>
  group_by(time, geo) |>
  summarize(msa = sum(msa),
            van = sum(van)) |>
  ungroup() |>
  mutate(psal = msa/van) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01") |>
  mutate(geo = countrycode::countrycode(geo, "eurostat", "iso3c" ))

ggplot(naa_ext) +
  aes(x=time, y=psal, col = geo) +
  geom_line(data = ~.x |> rename(GEO = geo) , aes(group=GEO), col = "gray75", linewidth = 0.25) +
  geom_line(show.legend=FALSE) +
  facet_wrap(vars(geo), ncol = 2) +
  scale_ofce_date() +
  scale_color_pays(list_iso3 = unique(naa_ext$geo)) +
  theme_ofce()