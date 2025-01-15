library(tidyverse)
library(eurostat)
library(ofce)
library(gt)
pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

earn.raw <- get_eurostat("earn_ses_annual", filters = list(isco08 = c("OC1-5", "OC9", "TOTAL"),
                                                       geo = pays2,
                                                       worktime = "FT",
                                                       nace_r2 = "B-S_X_O",
                                                       age = c("Y_LT30", "TOTAL"),
                                                       sex = c("M", "F", "T"),
                                                       indic_se = c("MED_E_PPS", "D9_E_PPS", "D1_E_PPS"))) |>
  select(-freq, -worktime, -nace_r2) |>
  drop_na() |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE"))) |>
  group_by(geo) |>
  filter(time==max(time)) |>
  group_by(geo) |>
  mutate(vr = values/values[age=="TOTAL"&sex=="T"&indic_se=="MED_E_PPS"]) |>
  ungroup() |>
  filter( sex == "T" | (age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS") ) |>
  filter( indic_se == "MED_E_PPS" | (age == "TOTAL" & isco08 == "TOTAL" & sex == "T") ) |>
  filter( age == "TOTAL" | (indic_se == "MED_E_PPS" & isco08 == "TOTAL" & sex == "T") )

earn_r <- earn.raw |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = vr)

earn <- earn.raw |>
  filter((age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS" & sex == "T")) |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = values) |>
  bind_rows(earn_r |> filter(! (age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS" & sex == "T") )) |>
  mutate(across(c(BE, DE), ~ ifelse(.x ==1, NA_real_, .x))) |>
  select(-time)

earn |>
  relocate(BE, NL, DE, FR, IT, ES) |>
  gt() |>
  fmt_percent(cols = c(BE, DE, ES, FR, NL, IT))

