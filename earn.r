library(tidyverse)
library(eurostat)
library(ofce)
library(gt)
pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

earn.raw <- get_eurostat("earn_ses_annual", filters = list(isco08 = c("OC1", "OC9", "TOTAL"),
                                                           geo = pays2,
                                                           worktime = c("TOTAL", "PT", "FT"),
                                                           nace_r2 = "B-S_X_O",
                                                           age = c("Y_LT30", "Y_GE50", "TOTAL"),
                                                           sex = c("M", "F", "T"),
                                                           indic_se = c("MED_E_PPS", "D9_E_PPS", "D1_E_PPS"))) |>
  select(-freq, -nace_r2) |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE"))) |>
  group_by(geo) |>
  filter(time==max(time)) |>
  group_by(geo) |>
  mutate(vr = values/values[age=="TOTAL"&sex=="T"&indic_se=="MED_E_PPS" &isco08 == "TOTAL" & worktime == "TOTAL"]) |>
  group_by(isco08, age, sex, indic_se, time, geo) |>
  mutate(ptp = (values[worktime == "FT"] - values[worktime == "TOTAL"]) / (values[worktime == "FT"] - values[worktime == "PT"])) |>
  ungroup() |>
  filter( sex == "T" | (age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS" & worktime == "TOTAL") ) |>
  filter( indic_se == "MED_E_PPS" | (age == "TOTAL" & isco08 == "TOTAL" & sex == "T" & worktime == "TOTAL") ) |>
  filter( age == "TOTAL" | (indic_se == "MED_E_PPS" & isco08 == "TOTAL" & sex == "T" & worktime == "TOTAL") )

earn_r <- earn.raw |>
  filter(worktime == "TOTAL") |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = vr) |>
  mutate(tt = "rel")

earn_p <- earn.raw |>
  filter(age == "TOTAL", isco08 == "TOTAL", indic_se == "MED_E_PPS", worktime == "TOTAL", sex %in% c("M", "F")) |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = ptp) |>
  mutate(tt = "pt")

earn <- earn.raw |>
  filter(age == "TOTAL", isco08 == "TOTAL", indic_se == "MED_E_PPS", sex == "T", worktime == "TOTAL") |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = values) |>
  mutate(tt = "ref") |>
  bind_rows(earn_p) |>
  bind_rows(earn_r |> filter(! (age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS" & sex == "T") )) |>
  mutate(across(c(BE, DE), ~ ifelse(.x ==1, NA_real_, .x))) |>
  select(-time) |>
  mutate(
    label = case_when(
      tt == "ref" ~ "Salaire médian brut annuel (référence)",
      tt == "pt" & sex == "M" ~ "Part des hommes à temps partiel",
      tt == "pt" & sex == "F" ~ "Part des femmes à temps partiel",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "D1_E_PPS" ~ "Salaire du premier décile",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "D9_E_PPS" ~ "Salaire du dernier décile",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "M" & indic_se == "MED_E_PPS" ~ "Salaire des hommes",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "F" & indic_se == "MED_E_PPS" ~ "Salaire des femmes",
      tt== "rel" & isco08 == "TOTAL" & age == "Y_LT30" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des moins de 30 ans",
      tt== "rel" & isco08 == "TOTAL" & age == "Y_GE50" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des plus de 50 ans",
      tt== "rel" & isco08 == "OC1" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des *managers*",
      tt== "rel" & isco08 == "OC9" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des travailleurs manuels peu qualifiés") )

return(earn)
