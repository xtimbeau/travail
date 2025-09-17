library(tidyverse)
library(eurostat)
library(ofce)

nace <- source_data("nace.r")$nace
m_a20 <- nace  |> pull(a20) |> unique()
m_a10 <- nace |> pull(a10) |> unique()

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")

adj <- c("SCA", "SA", "CA", "NSA")

label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

naq10_e <- source_data("naq10_e")$naq_e

naa_a64 <- get_eurostat("nama_10_a64", filters = list(unit = "CP_MEUR",
                                                      nace_r2  = m_a20,
                                                      na_item = c("B1G", "D1", "D11", "P51C", "D29X39"),
                                                      geo = pays2) ) |>
  drop_na(values) |>
  pivot_wider(id_cols = c(nace_r2, geo, time),
              names_from =  na_item, values_from = values) |>
  left_join(nace |> select(a10, a20, md = marchand, hi = hors_imm, hifi), by = c("nace_r2"= "a20")) |>
  group_by(a10, geo, time) |>
  summarise(across(c(B1G, D1, D11, P51C, D29X39), sum),
            across(c(md, hifi, hi), first),
            .groups = "drop") |>
  rename(nace_r2 = a10) |>
  mutate(across(c(D1, D11, P51C, D29X39), ~ .x / B1G)) |>
  rename_with(tolower, c(D1, D11, P51C, D29X39)) |>
  cross_join(tibble(month = seq(1, 10, by=3))) |>
  mutate(time = ym(str_c(year(time), " ", month))) |>
  rename(B1Ga = B1G) |>
  select(-month)

naq_a10 <- get_eurostat("namq_10_a10", filters = list(unit = "CP_MEUR",
                                                      nace_r2  = m_a10,
                                                      na_item = c("B1G", "D1", "D11"),
                                                      geo = pays2) ) |>
  select(na_item, geo, time, values, nace_r2, s_adj) |>
  mutate(s_adj = factor(s_adj, adj)) |>
  arrange(s_adj) |>
  group_by(geo, time, nace_r2, na_item) |>
  drop_na(values) |>
  summarize(across(c(values, s_adj), first),
            .groups = "drop") |>
  pivot_wider(id_cols = c(nace_r2, geo, time),
              names_from =  na_item, values_from = c(values, s_adj)) |>
  rename_with(~str_remove(.x, "values_")) |>
  left_join(naq10_e, by = c("geo", "time", "nace_r2")) |>
  left_join(naa_a64, by = c("geo", "time", "nace_r2")) |>
  group_by(geo, nace_r2) |>
  arrange(time) |>
  mutate(d1 = D1/B1G) |>
  fill(p51c, d29x39, d1) |>
  ungroup() |>
  mutate(
    van = B1G*(1-p51c),
    vab = B1G,
    msa = d1 * B1G * tsal,
    msanc = d1 * B1G,
    ip = B1G * d29x39)

naq <- naq_a10 |>
  group_by(geo, time) |>
  summarize(
    across(c(van, vab, msa, msanc, ip), ~sum(.x[md], na.rm=TRUE), .names = "{.col}_md"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x[hi&md], na.rm=TRUE), .names = "{.col}_mdhi"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x[hifi&md], na.rm=TRUE), .names = "{.col}_mdhifi"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x, na.rm=TRUE), .names = "{.col}_tb"),
    .groups = "drop") |>
  pivot_longer(starts_with(c("van", "vab", "msa", "ip"))) |>
  separate(name, into = c("var", "champ"), sep = "_") |>
  pivot_wider(names_from = var, values_from = value) |>
  mutate(
    psal = msa/van,
    psalb = msa/vab,
    psalnc = msanc/van,
    psalbnc = msanc/vab,) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01") |>
  mutate(geo = factor(geo,  c("DE", "FR", "IT", "ES", "NL", "BE")))

naa <- naq |>
  mutate(year = year(time)) |>
  group_by(geo, year, champ) |>
  summarize(
    nq = n(),
    across(c(van, vab, msa, msanc, ip), ~sum(.x, na.rm=TRUE)*4/nq),
    .groups = "drop") |>
  ungroup() |>
  mutate(
    psal = msa/van,
    psalb = msa/vab,
    psalnc = msanc/van,
    psalbnc = msanc/vab,
    time = ym(str_c(year, "-01"))) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01") |>
  mutate(geo = factor(geo,  c("DE", "FR", "IT", "ES", "NL", "BE")))

na_tot <- get_eurostat("nama_10_gdp",
                       filters = list(geo = pays2,
                                      na_item = c("D21", "D31"),
                                      unit = "CP_MEUR")) |>
  drop_na() |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time, d2131 = D21 - D31)
max_y <- max(year(na_tot$time))

nq_tot <- get_eurostat("namq_10_gdp",
                       filters = list(geo = pays2,
                                      na_item = c("D21X31"),
                                      unit = "CP_MEUR")) |>
  drop_na() |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  arrange(s_adj) |>
  group_by(geo, time, na_item) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  filter(year(time) > max_y ) |>
  group_by(geo, na_item) |>
  summarize(values = mean(values, na.rm=TRUE) * 4,
            .groups = "drop") |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time = ym(str_c(max_y+1, "-01")), d2131 = D21X31) |>
  bind_rows(na_tot) |>
  arrange(geo, time)

d51 <- get_eurostat("nasa_10_nf_tr",
                    filters = list(na_item = c("B1G", "D51"), direct = "PAID",
                                   geo = pays2, unit = "CP_MEUR",
                                   sector = c("S11", "S12"))) |>
  drop_na() |>
  pivot_wider(names_from = sector, values_from = values) |>
  mutate(tt = S11 + S12, md = S11 + S12, hi = NA, hifi = S11) |>
  pivot_longer(c(tt, md, hi, hifi), names_to = "champ", values_to = "value") |>
  select(-S11, -S12) |>
  pivot_wider(names_from = na_item, values_from = value) |>
  select(geo, time, is=D51, B1Ga = B1G, champ) |>
  right_join(naa |> select(time, geo, B1G = vab, van, msa, ip, champ), by = c("time", "geo", "champ")) |>
  mutate(t2is = is/B1G,
         tis = is/(van-msa-ip)) |>
  group_by(geo, champ) |>
  fill(tis, t2is) |>
  transmute(
    geo, time, tis, t2is, champ,
    is = tis*(van-msa-ip),
    is2 = t2is*B1G)

naa_ext <- naa |>
  select(geo, time, van, vab, msa, msanc, ip, champ) |>
  arrange(time, geo, champ) |>
  left_join(d51, by  = c("time", "geo")) |>
  mutate(psal = msa/van,
         psalnc = msanc/van,
         psalb = msa/vab,
         psalncb = msanc/vab) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01") |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE"))) |>
  mutate(
    tp = (van - msa - ip - is)/van,
    tpb = (van - msa)/van )

assets <- source_data("assets.r")$assets |>
  pivot_longer(cols = c(BE, DE, ES, FR, IT, NL), values_to = "asset", names_to = "geo") |>
  filter(asset>0)

naa_ext2 <- naa_ext |>
  filter(year(time)<= max_y) |>
  left_join( assets, by =c("geo", "time") ) |>
  mutate(r = tp*van/asset) |>
  arrange( desc(time), geo) |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))

return(list(naa = naa_ext, naaa = naa_ext2))
