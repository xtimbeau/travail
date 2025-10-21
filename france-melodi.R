library(tidyverse)
library(ofce)
library(melodi)

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

TES_branches <- c("A", "B_D_E",
                  "C10T12", "C13T15", "C16T18", "C19", "C20", "C21", "C22_23", "C24_25", "C26T28", "C29_30", "C31T33",
                  "F", "G", "H", "I", "J", "K", "L", "M_N", "OTQ", "R", "S")

fr_tes_L <- melodi::get_all_data("DD_CNA_SUT") |>
  filter(
    STO == "P2", PRICES=="V", UNIT_MEASURE=="XDC", PRODUCT=="CPA_L",
    ACTIVITY %in% TES_branches) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    nace_r2 = ACTIVITY,
    p2_l = OBS_VALUE ) |>
  arrange(time)

fr_emp <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("SAL", "SELF", "EMP"),
         UNIT_MEASURE == "FT",
         TRANSFORMATION == "N",
         ACTIVITY %in% TES_branches) |>
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
         ACTIVITY %in% TES_branches) |>
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
  mutate(loyers = L68R1A+L68I0A+L68I0B+L68R1B) |>
  arrange(time)

assets <- "DD_CNA_PATRIMOINE_BRANCHES" |>
  melodi::get_all_data() |>
  filter(ACTIVITY %in% TES_branches,
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
