library(tidyverse)
library(ofce)
library(melodi)

br <- tibble::tribble(
  ~nace_r2, ~md, ~mdhi, ~nace,
  "A", TRUE, TRUE, "A",
  "B_D_E", TRUE, TRUE, "B_D_E",
  "C10T12", TRUE, TRUE, "C",
  "C13T15", TRUE, TRUE, "C",
  "C16T18", TRUE, TRUE, "C",
  "C19", TRUE, TRUE, "C",
  "C20", TRUE, TRUE, "C",
  "C21", TRUE, TRUE, "C",
  "C22_23", TRUE, TRUE, "C",
  "C24_25", TRUE, TRUE, "C",
  "C26T28", TRUE, TRUE, "C",
  "C29_30", TRUE, TRUE, "C",
  "C31T33", TRUE, TRUE, "C",
  "F", TRUE, TRUE, "F",
  "G", TRUE, TRUE, "G",
  "H", TRUE, TRUE, "H",
  "I", TRUE, TRUE, "I",
  "J", TRUE, TRUE, "J",
  "K", TRUE, TRUE,  "K",
  "L", TRUE, FALSE, "L",
  "M_N", TRUE, TRUE, "M_N",
  "OTQ", FALSE, FALSE, "OTQ",
  "R", TRUE, TRUE, "R",
  "S", TRUE, TRUE, "S" )
codes <- vroom::vroom("ACTIVITY.codes.csv")
colnames(codes) <- c("nace_r2", "fr", "en")
codes <- codes |>
  bind_rows(tibble(code = "C", fr="Industrie manufacturière", en="Manufacturing"))

info_melodi <- function(data) {
  map(names(data |> select(-OBS_VALUE, -TIME_PERIOD)), ~count(data, across(all_of(.x))))
}

nace <- source_data("nace.r")$nace
a20 <- nace$a20
pays <- source_data("nace.r")$pays1

vahim <- source_data("vaq.r")$naa |>
  select(geo, time, van, champ) |>
  filter(champ %in% c("mdhim", "mdhi"), geo %in% pays) |>
  pivot_wider(names_from = champ, values_from = van) |>
  mutate(r = (mdhim-mdhi)/mdhi)

# ggplot(vahim) +
#   aes(x=time, y=r, color = geo, group = geo)+
#   geom_line(layout = "fixed", color = "gray85") +
#   geom_line() +
#   facet_wrap(vars(geo)) +
#   scale_color_pays("eurostat") +
#   theme_ofce()

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
  select(geo, time, CI_L) |>
  left_join(vahim, by=c("geo", "time")) |>
  mutate(cil = CI_L/mdhi)

# uses |>
#   mutate(r2 = CI_L/mdhi) |>
#   drop_na() |>
#   ggplot()+
#   aes(x=time, y=r2, color = geo, group = geo)+
#   geom_line(layout = "fixed", color = "gray85") +
#   geom_line() +
#   geom_line(aes(y=r), linetype = "11") +
#   facet_wrap(vars(geo)) +
#   scale_color_pays("eurostat") +
#   theme_ofce()

fr_tes_L <- melodi::get_all_data("DD_CNA_SUT") |>
  filter(
    STO == "P2", PRICES=="V", UNIT_MEASURE=="XDC", PRODUCT=="CPA_L",
    ACTIVITY %in% br$nace_r2) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    nace_r2 = ACTIVITY,
    p2l = OBS_VALUE ) |>
  arrange(time)

fr_emp <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("SAL", "SELF", "EMP"),
         UNIT_MEASURE == "FT",
         TRANSFORMATION == "N",
         ACTIVITY %in% br$nace_r2) |>
  transmute(time = ymd(TIME_PERIOD, truncated=2),
            value = OBS_VALUE,
            STO,
            nace_r2 = ACTIVITY) |>
  filter(time>="1978-01-01") |>
  pivot_wider(names_from = STO, values_from = value) |>
  mutate(
    across(c(EMP, SELF, SAL), ~replace_na(.x, 0)) ) |>
  arrange(time, nace_r2)

fr_branches <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("B1G", "P51C", "D1", "D29X39", "D21X31"),
         PRICES == "V",
         UNIT_MEASURE == "XDC",
         TRANSFORMATION == "N",
         ACTIVITY %in% br$nace_r2) |>
  transmute(time = ymd(TIME_PERIOD, truncated=2),
            value = OBS_VALUE,
            nace_r2 = ACTIVITY,
            na_item = STO,
            ae = ACCOUNTING_ENTRY,
            ca = COUNTERPART_AREA) |>
  filter(time>="1978-01-01") |>
  arrange(time, na_item, nace_r2) |>
  pivot_wider(names_from = c(ae, ca) , values_from = value) |>
  rowwise() |>
  mutate(value = coalesce(B_W0, B_W2, D_W2, D_W0)) |>
  select(time, nace_r2, na_item, value) |>
  pivot_wider(names_from = na_item, values_from = value) |>
  rename_with(tolower) |>
  left_join(fr_tes_L, by = c("nace_r2", "time"))

men <- "DD_CNA_CONSO_MENAGES_PRODUITS" |>
  melodi::get_all_data() |>
  filter(
    PRODUCT %in% c("HI_L68I0A_B2020", "HI_L68I0B_B2020", "HI_L68R1A_B2020", "HI_L68R1B_B2020"),
    UNIT_MEASURE == "XDC",
    TRANSFORMATION == "N",
    STO == "P31",
    REF_SECTOR == "S14",
    PRICES=="V") |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    p31 = OBS_VALUE,
    PRODUCT = PRODUCT |> str_remove("HI_") |> str_remove("_B2020") ) |>
  pivot_wider(names_from = PRODUCT, values_from = p31) |>
  transmute(
    time, nace_r2 = "L",
    lymen = L68R1A+L68I0A+L68I0B+L68R1B,
    lymenimp = L68I0A+L68I0B,
    lymenree = L68R1A+L68R1B) |>
  arrange(desc(time))

assets <- "DD_CNA_PATRIMOINE_BRANCHES" |>
  melodi::get_all_data() |>
  filter(ACTIVITY %in% br$nace_r2,
         STO == "LE", PRICES=="U", COUNTERPART_AREA == "W0",
         INSTR_ASSET %in% c("N1N", "N111N")) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    value = OBS_VALUE,
    nace_r2 = ACTIVITY,
    asset = INSTR_ASSET) |>
  arrange(desc(time), nace_r2, asset) |>
  pivot_wider(names_from = asset, values_from = value) |>
  mutate(across(c(N1N, N111N), ~replace_na(.x, 0)))

melodi <- fr_branches |>
  left_join(fr_emp, by = c("time", "nace_r2")) |>
  left_join(assets, by = c("time", "nace_r2")) |>
  left_join(men, by = c("time", "nace_r2")) |>
  left_join(br, by = "nace_r2") |>
  rename_with(tolower) |>
  mutate(
    d29x39 = replace_na(d29x39, 0)  ) |>
  mutate(
    vab = b1g,
    van = b1g - p51c,
    psal = (d1 * (1 + self/sal)) / van,
    p2lf = p2l[time=="1980-01-01"]/van[time=="1980-01-01"] * van,
    tp = (van - d1 * (1 + self/sal) - d29x39) / van,
    rb = (van - d1 * (1 + self/sal) - d29x39) / n1n)

dvalL <- melodi |>
  group_by(time) |>
  summarize(dva = -sum(p2l)+sum(p2lf)) |>
  mutate(nace_r2 = "L")
dval <- melodi |>
  filter(nace_r2 != "L") |>
  mutate(dva = p2l - p2lf) |>
  select(time, nace_r2, dva) |>
  bind_rows(dvalL)

melodi <- melodi |>
  left_join(dval, by=c("time", "nace_r2")) |>
  mutate(
    rb2 = (van + dva - d1 * (1 + self/sal) - d29x39) / n1n)

melodi2 <- melodi |>
  group_by(time, nace) |>
  summarize(
    across(c(b1g, d1, d29x39, p51c, p2l, p2lf, emp, self, sal, n111n, n1n, lymen, dva), sum),
    across(c(md, mdhi), first),
    .groups = "drop") |>
  mutate(
    vab = b1g,
    van = b1g - p51c,
    psal = (d1 * (1 + self/sal)) / van,
    tp = (van - d1 * (1 + self/sal) - d29x39) / van,
    rb = (van - d1 * (1 + self/sal) - d29x39) / n1n,
    rb2 = (van + dva - d1 * (1 + self/sal) - d29x39) / n1n)
ssi <- melodi2 |> filter(time == "2024-01-01",md) |> select(time, van, nace, md) |> mutate(van=van/sum(van))

melodi_m <- melodi |>
  group_by(time) |>
  summarize(
    across(c(b1g, d1, d29x39, p51c, p2l, p2lf, emp, self, sal, n111n, n1n, lymen, van, vab, dva), ~sum(.x[md]), .names = "{.col}_md" ),
    across(c(b1g, d1, d29x39, p51c, p2l, p2lf, emp, self, sal, n111n, n1n, lymen, van, vab, dva), ~sum(.x[mdhi]), .names = "{.col}_mdhi" )) |>
  pivot_longer(cols = ends_with(c("md", "mdhi"))) |>
  separate(name, sep="_", into = c("var", "champ")) |>
  pivot_wider(names_from = var, values_from = value) |>
  group_by(champ) |>
  mutate(
    psal = (d1 * (1 + self/sal)) / van,
    tp = (van - d1 * (1 + self/sal) - d29x39) / van,
    rb = (van - d1 * (1 + self/sal) - d29x39) / n1n,
    rb2 = (van + dva - d1 * (1 + self/sal) - d29x39) / n1n)

return(list(full = melodi, a20 = melodi2, aggr = melodi_m))

# ggplot(melodi |> filter(md, time>="2000-01-01") )+
#   aes(x=time) +
#   geom_line(aes(y = rb), linewidth=0.75, color = "steelblue1" ) +
#   geom_line(aes(y = rb2), color = "pink2", linewidth=0.75, linetype = "11") +
#   ggbraid::geom_braid(aes(ymin = rb, ymax = rb2), color=NA, fill="palegreen2", alpha=0.25) +
#   theme_ofce()+
#   facet_wrap(vars(nace_r2))+scale_y_continuous(limits =c(-0.35, 0.35), oob=scales::oob_keep)
#
# lbl_nace <- as_labeller(codes |> pull(fr, name=nace_r2))
# ggplot(melodi2 |> filter(md, time>="2000-01-01") )+
#   aes(x=time) +
#   geom_point(
#     data=ssi |> filter(md),
#     aes(y = -0.18, size=van*50), color = "pink", alpha=0.5) +
#   geom_marquee(
#     data=ssi |> filter(md),
#     aes(y = -0.18, label = str_c(round(100*van),"%")),
#     color = "black",
#     size=7, size.unit="pt") +
#   scale_size_identity()+
#   geom_line(aes(y = rb), linewidth=0.75, color = "steelblue1" ) +
#   geom_line(aes(y = rb2), color = "pink2", linewidth=0.75, linetype = "11") +
#   ggbraid::geom_braid(aes(ymin = rb, ymax = rb2, fill=rb2>rb), color=NA, alpha=0.25, show.legend=FALSE) +
#   scale_fill_manual(values=c("red", "palegreen"))+
#   theme_ofce()+
#   facet_wrap(vars(nace), labeller = lbl_nace )+
#   scale_y_continuous(limits =c(-0.3, 0.3), oob=scales::oob_keep)
#
# annotations <- tribble(
#   ~time, ~y, ~texte,
#   "2008-10-01", 0.1, "Rendement du capital productif corrigé des consommations intermédiaires en services immobiliers auprès des entreprises",
#   "1988-03-01", 0.07, "Rendement du capital productif des branches marchandes hors immobilier",
# ) |> mutate(time=ymd(time))
#
# ggplot(melodi_m |> filter(champ=="mdhi"))+
#   aes(x=time) +
#   geom_line(aes(y = rb), linewidth=0.75, color = "steelblue1" ) +
#   geom_line(aes(y = rb2), color = "pink2", linewidth=0.75, linetype = "11") +
#   ggbraid::geom_braid(aes(ymin = rb, ymax = rb2, fill=rb2>rb), color=NA, alpha=0.25, show.legend=FALSE) +
#   scale_fill_manual(values=c("red", "palegreen"))+
#   theme_ofce()+
#   geom_marquee(
#     data=annotations,
#     aes(label=texte, y=y),
#     size=9, size.unit="pt", hjust = 0, vjust=1, lineheight = 1.1, width=unit(6, "cm")) +
#   scale_y_continuous(labels = scales::label_percent(1),
#                      breaks = scales::pretty_breaks(10))+
#   scale_ofce_date()+
#   labs(x=NULL, y="% du stoick de capital")
#
# ggplot(melodi_m )+geom_line(aes(x=time, y = p2l/n1n, color = champ))
