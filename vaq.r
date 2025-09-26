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

naa_a64 <- "nama_10_a64" |>
  get_eurostat(filters = list(unit = "CP_MEUR",
                              nace_r2  = m_a20,
                              na_item = c("B1G", "D1", "D11", "P51C", "D29X39"),
                              geo = pays2) ) |>
  drop_na(values) |>
  pivot_wider(id_cols = c(nace_r2, geo, time),
              names_from =  na_item, values_from = values) |>
  left_join(nace |>
              select(a10, a20,),
            by = c("nace_r2"= "a20")) |>
  group_by(a10, geo, time) |>
  summarise(across(c(B1G, D1, D11, P51C, D29X39), sum),
            .groups = "drop") |>
  rename(nace_r2 = a10) |>
  mutate(across(c(D1, D11, P51C, D29X39), ~ .x / B1G)) |>
  rename_with(tolower, c(D1, D11, P51C, D29X39)) |>
  cross_join(tibble(month = seq(1, 10, by=3))) |>
  mutate(time = ym(str_c(year(time), " ", month))) |>
  rename(B1Ga = B1G) |>
  select(-month)

naq_a10 <- "namq_10_a10" |>
  get_eurostat(filters = list(unit = "CP_MEUR",
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
    ip = B1G * d29x39) |>
  left_join(nace |>
              distinct(a10, .keep_all = TRUE ) |>
              select(nace_r2 = a10, md=marchand, hi = hors_imm, hifi, hfi),
            by = "nace_r2")

naq_L <- naq_a10 |>
  filter(nace_r2 == "L") |>
  mutate(year = year(time),
         D29X39 = B1G * d29x39) |>
  group_by(geo, year) |>
  summarize(
    nq = n(),
    across(c(B1G, D1, D29X39), ~sum(.x, na.rm=TRUE)*4/nq),
    .groups = "drop") |>
  mutate(time = ym(str_c(year, "-01"))) |>
  select(-year) |>
  rename(B1Gq = B1G,
         D1q = D1,
         D29X39q = D29X39)

L68A <- "nama_10_a64" |>
  get_eurostat(filters = list(unit = "CP_MEUR",
                              nace_r2  = c("L", "L68A"),
                              na_item = c("B1G", "D1", "D11", "P51C", "D29X39"),
                              geo = pays2) ) |>
  drop_na(values) |>
  pivot_wider(names_from = c(na_item, nace_r2), values_from = values) |>
  group_by(geo) |>
  mutate(
    rb1g = B1G_L68A/B1G_L,
    p51c = ifelse(is.na(P51C_L68A), P51C_L*rb1g, P51C_L68A),
    d1 = 0,
    d29x39 = ifelse(is.na(D29X39_L68A), D29X39_L*rb1g, D29X39_L68A) ) |>
  transmute(time, geo,
            rb1g,
            B1G = B1G_L - B1G_L68A,
            B1G_L,
            P51C = P51C_L - p51c,
            D1 = D1_L - d1,
            D29X39 = D29X39_L - d29x39) |>
  right_join(naq_L, by = c("time", "geo")) |>
  arrange(geo, time) |>
  group_by(geo) |>
  mutate(rp51c = P51C/B1G,
         rd29 = D29X39/B1G,
         rb1g = B1G/B1Gq) |>
  fill(c(rp51c, rd29, rb1g)) |>
  mutate(
    nace_r2 = 'Lbis',
    B1G = rb1g * B1Gq,
    P51C = rp51c * B1G,
    D29X39 = rd29 * B1G,
    rB1G = B1G/B1Gq,
    rP51C = P51C/B1G,
    rD29X39 = D29X39/B1G )

L68Aq <- L68A |>
  mutate(year = year(time)) |>
  select(geo, year, rP51C, rD29X39, rB1G, nace_r2) |>
  left_join(naq_a10 |> filter(nace_r2 == "L") |> transmute(time, year=year(time), geo, B1G, msa, msanc),
            by = c("geo", "year")) |>
  transmute(
    nace_r2, time, geo,
    md = FALSE, hi = FALSE, hifi = FALSE, hfi = FALSE,
    him = TRUE,
    vab = B1G * rB1G,
    van = vab - rP51C * vab,
    ip = rD29X39 * vab,
    msa, msanc)

naq <- naq_a10 |>
  mutate(him = hi&md) |>
  bind_rows(L68Aq) |>
  group_by(geo, time) |>
  summarize(
    across(c(van, vab, msa, msanc, ip), ~sum(.x[md], na.rm=TRUE), .names = "{.col}_md"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x[hi&md], na.rm=TRUE), .names = "{.col}_mdhi"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x[hifi&md], na.rm=TRUE), .names = "{.col}_mdhifi"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x[hfi&md], na.rm=TRUE), .names = "{.col}_mdhfi"),
    across(c(van, vab, msa, msanc, ip), ~sum(.x[him], na.rm=TRUE), .names = "{.col}_mdhim"),
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

na_tot <-  "nama_10_gdp" |>
  get_eurostat(
    filters = list(geo = pays2,
                   na_item = c("D21", "D31"),
                   unit = "CP_MEUR")) |>
  drop_na() |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time, d2131 = D21 - D31)

# men <- "nasq_10_nf_tr" |>
#   get_eurostat(
#     filters = list(unit = "CP_MEUR",
#                    sector = c("S14"),
#                    s_adj = c("SCA", "NSA"),
#                    na_item = c("B1G", "B1N", "D1", "D5", "D4", "D42_TO_D45", "D41", "D42", "D3", "D2"),
#                    geo = pays2) )


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

d51 <- "nasa_10_nf_tr" |>
  get_eurostat(
    filters = list(na_item = c("B1G", "D51"), direct = "PAID",
                   geo = pays2, unit = "CP_MEUR",
                   sector = c("S11", "S12"))) |>
  drop_na() |>
  pivot_wider(names_from = sector, values_from = values) |>
  mutate(tb = S11 + S12, md = S11 + S12, mdhi = S11+S12, mdhifi = S11, mdhfi = S11) |>
  pivot_longer(c(tb, md, mdhi, mdhifi, mdhfi), names_to = "champ", values_to = "value") |>
  select(-S11, -S12) |>
  pivot_wider(names_from = na_item, values_from = value) |>
  select(geo, time, is=D51, B1Ga = B1G, champ) |>
  right_join(naa |> select(time, geo, B1G = vab, van, msa, ip, champ), by = c("time", "geo", "champ")) |>
  mutate(t2is = is/B1G,
         tis = is/(van-msa-ip)) |>
  group_by(geo, champ) |>
  fill(tis, t2is) |>
  ungroup() |>
  transmute(
    geo, time, tis, t2is, champ,
    is = tis*(van-msa-ip),
    is2 = t2is*B1G)

naa_ext <- naa |>
  select(geo, time, van, vab, msa, msanc, ip, champ) |>
  arrange(time, geo, champ) |>
  left_join(d51, by  = c("time", "geo", "champ")) |>
  mutate(psal = msa/van,
         psalnc = msanc/van,
         psalb = msa/vab,
         psalncb = msanc/vab) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01") |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE"))) |>
  mutate(
    tp = (van - msa - ip - is)/van,
    tpb = (van - msa)/van )

assets <- source_data("assets.r")$assets  |>
  filter(assets>0)

naa_ext2 <- naa_ext |>
  filter(year(time)<= max_y) |>
  left_join( assets, by =c("geo", "time", "champ") ) |>
  mutate(r = tp*van/assets) |>
  arrange( desc(time), geo) |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))

return(list(naa = naa_ext, naaa = naa_ext2))
