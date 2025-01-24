library(tidyverse)
library(eurostat)
library(ofce)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

eq <- get_eurostat("namq_10_a10_e",
                   filters = list(na_item = "SAL_DC",
                                  geo = pays2,
                                  unit = "THS_PER")) |>
  drop_na(values) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  group_by(across(-c(s_adj, values))) |>
  arrange(s_adj) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  select(geo, time, nace_r2, emp_sal = values)

d1 <- get_eurostat("namq_10_a10",
                   filters = list(na_item = c("D1", "D11", "D12"),
                                  geo = pays2,
                                  unit = "CP_MEUR")) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  drop_na() |>
  group_by(across(-c(s_adj, values))) |>
  arrange(s_adj) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  select(geo, time, values, nace_r2, na_item) |>
  pivot_wider(names_from = na_item, values_from = values)

pc <- get_eurostat("namq_10_fcs", filters = list(na_item = "P31_S14", unit = c("CP_MEUR", "CLV20_MEUR"), geo = pays2)) |>
  drop_na(values) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  group_by(across(-c(s_adj, values))) |>
  arrange(s_adj) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  select(geo, time, values, unit) |>
  pivot_wider(names_from = unit, values_from = values) |>
  mutate(pc = CP_MEUR/CLV20_MEUR) |>
  select(geo, time, pc)

salaires <- eq |>
  left_join(d1, by = c("time", "geo", "nace_r2")) |>
  group_by(geo, time) |>
  filter(nace_r2 != "TOTAL") |>
  summarize(w = sum(D1)/sum(emp_sal),
            wbrut = sum(D11)/sum(emp_sal),
            w_md = (sum(D1)- D1[nace_r2 == "O-Q"])/(sum(emp_sal)- emp_sal[nace_r2 == "O-Q"]),
            w_nmd = (D1[nace_r2 == "O-Q"])/(emp_sal[nace_r2 == "O-Q"]) ,
            .groups = "drop") |>
  left_join(pc, by = c("time", "geo")) |>
  group_by(geo) |>
  mutate(wr = 4*slider::slide_dbl(w, .before = 3, .f = mean)/pc,
         wbr = 4*slider::slide_dbl(wbrut, .before = 3, .f = mean)/pc,
         wr_md = 4*slider::slide_dbl(w_md, .before = 3, .f = mean)/pc,
         wr_nmd = 4*slider::slide_dbl(w_nmd, .before = 3, .f = mean)/pc) |>
  ungroup() |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))

return(salaires)
