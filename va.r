library(tidyverse)
library(eurostat)
library(ofce)

marchand <- source_data("nace.r")$marchand
marchand2 <- source_data("nace.r")$marchand2
nace <- source_data("nace.r")$nace

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")

label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

naa_e <- source_data("naa_e")$naa_e

naa_a20 <- get_eurostat("nama_10_a64", filters = list(unit = "CP_MEUR",
                                                      nace_r2  = marchand,
                                                      na_item = c("B1G", "D1", "D11", "P51C", "D29X39"),
                                                      geo = pays2) ) |>
  select(na_item, geo, time, values, nace_r2) |>
  drop_na(values) |>
  pivot_wider(id_cols = c(nace_r2, geo, time), names_from =  na_item, values_from = values ) |>
  left_join(naa_e, by = c("geo", "time", "nace_r2")) |>
  left_join(nace |> select(nace_r2 = a20, a10), by = "nace_r2" ) |>
  mutate(
    van = B1G-P51C,
    vab = B1G,
    msa = D1 * tsal,
    msa2 = D1,
    ip = D29X39)

naa_a10 <- naa_a20 |>
  group_by(geo, time, a10) |>
  summarize(
    van = sum(van, na.rm = TRUE),
    vab = sum(vab, na.rm = TRUE),
    msa = sum(msa, na.rm = TRUE),
    msa2 = sum(msa2, na.rm = TRUE),
    ip = sum(ip, na.rm = TRUE),
    .groups = "drop")

naa <- naa_a20 |>
  group_by(geo, time) |>
  summarize(van = sum(van, na.rm=TRUE),
            vab = sum(vab, na.rm = TRUE),
            msa = sum(msa, na.rm=TRUE),
            msa2 = sum(msa2, na.rm = TRUE),
            ip = sum(ip, na.rm = TRUE),
            .groups = "drop") |>
  ungroup() |>
  mutate(psal = msa/van,
         psalb = msa/vab) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01")

max_y <- max(year(naa_a10$time))

naq <- get_eurostat("namq_10_a10", filters = list(unit = "CP_MEUR",  na_item = c("B1G", "D1"), nace_r2  = marchand2, geo = pays2)) |>
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

na_tot <- get_eurostat("nama_10_gdp", filters = list(geo = pays2, na_item = c("D21", "D31"), unit = "CP_MEUR")) |>
  drop_na() |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time, d2131 = D21 - D31)

nq_tot <- get_eurostat("namq_10_gdp", filters = list(geo = pays2, na_item = c("D21X31"), unit = "CP_MEUR")) |>
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

d51 <- get_eurostat("nasa_10_nf_tr", filters = list(na_item = "D51", direct = "PAID", geo = pays2, unit = "CP_MEUR")) |>
  drop_na() |>
  pivot_wider(names_from = sector, values_from = values) |>
  mutate(is = S11 + S12) |>
  select(geo, time, is)

naa_ext <- naa_a10 |>
  select(nace_r2 = a10, geo, time, van, vab, msa, msa2, ip) |>
  bind_rows(naq |> rename(vana=van, msaa = msa)) |>
  arrange(time, geo, nace_r2) |>
  group_by(geo, nace_r2) |>
  mutate(
    vana = lag(van)*vana,
    msa2a = lag(msa2)*msaa,
    msaa = lag(msa)*msaa,
    van = ifelse(is.na(van), vana, van),
    msa = ifelse(is.na(msa), msaa, msa),
    msa2 = ifelse(is.na(msa2), msa2a, msa2)) |>
  select(-msaa, -vana) |>
  group_by(time, geo) |>
  summarize(msa = sum(msa),
            msa2 = sum(msa2),
            van = sum(van),
            vab = sum(vab),
            ip = sum(ip)) |>
  ungroup() |>
  mutate(psal = msa/van,
         psal2 = msa2/van,
         psalb = msa/vab) |>
  filter(geo %in% c("DE", "FR", "IT", "ES", "NL", "BE"), time >= "1995-01-01") |>
  left_join(d51, by  = c("time", "geo")) |>
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
  arrange( desc(time), geo)

return(list(naa = naa_ext, naaa = naa_ext2))
