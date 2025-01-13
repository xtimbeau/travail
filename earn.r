library(tidyverse)
library(eurostat)
library(ofce)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

earn <- get_eurostat("earn_ses_annual", filters = list(isco08 = c("OC1-5", "OC6-8", "OC7-9", "TOTAL"),
                                                       geo = pays2,
                                                       worktime = "FT",
                                                       nace_r2 = "B-S_X_O",
                                                       age = c("Y20-29", "TOTAL"),
                                                       sex = c("M", "F", "T"),
                                                       indic_se = c("MED_E_PPS", "D9_E_PPS", "D1_E_PPS"))) |>
  drop_na() |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE"))) |>
  group_by(geo) |>
  filter(time==max(time)) |>
  pivot_wider(names_from = geo, values_from = values)


