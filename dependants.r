library(eurostat)
library(ofce)
library(tidyverse)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

pop <- get_eurostat("demo_pjan", filters = list(geo = pays2, sex = c("M", "F"))) |>
  mutate(pop = values/1000) |>
  mutate(nage = str_remove(age, "Y") |> as.numeric(),
         nage = ifelse(age ==  "Y_LT1", 0, nage),
         nage = ifelse(age == "Y_GE100", 100, nage)) |>
  filter(!is.na(nage)) |>
  mutate(gage = case_when(
    nage <= 14 ~ 'Y_LT14',
    between(nage, 15, 24) ~ "Y15-24",
    between(nage, 25, 54) ~ "Y25-54",
    between(nage, 55, 64) ~ "Y55-64",
    nage>=65 ~ "Y_GE65" )) |>
  group_by(geo, time, sex, gage) |>
  summarize(pop = sum(pop, na.rm=TRUE))|>
  ungroup()

proj <- get_eurostat("proj_23np", filters = list(geo = pays2, sex = c("M", "F"), unit = "PER", projection ="BSL")) |>
  mutate(pop = values/1000) |>
  mutate(nage = str_remove(age, "Y") |> as.numeric(),
         nage = ifelse(age ==  "Y_LT1", 0, nage),
         nage = ifelse(age == "Y_GE100", 100, nage)) |>
  filter(!is.na(nage)) |>
  mutate(gage = case_when(
    nage <= 14 ~ 'Y_LT14',
    between(nage, 15, 24) ~ "Y15-24",
    between(nage, 25, 54) ~ "Y25-54",
    between(nage, 55, 64) ~ "Y55-64",
    nage>=65 ~ "Y_GE65" )) |>
  group_by(geo, time, sex, gage) |>
  summarize(pop = sum(pop, na.rm=TRUE)) |>
  ungroup()

max_pop <- max(pop$time)
popj <- bind_rows(pop, proj |> filter(time>max_pop)) |>
  arrange(geo, time, sex) |>
  rename(age = gage)

active <- get_eurostat("lfsi_emp_a",
                       filters = list(geo = pays2, age = c("Y15-24", "Y25-54", "Y55-64"), unit = "THS_PER", sex = c("M", "F"), indic_em = "EMP_LFS")) |>
  mutate(emp = values) |>
  drop_na(emp)

minmin_active <- active |> group_by(geo) |> summarize(time=min(time)) |> summarize(time=max(time)) |> pull(time)

active <- active |>
  filter(time>=minmin_active)

full <- full_join(popj |> select(time, geo, sex, age, pop) |> filter(time>= min(active$time)), active |> select(time, geo, sex, age, emp), by = c("sex", "age", "geo", "time")) |>
  mutate(emp = ifelse(age %in% c("Y_LT14", "Y_GE65"), 0, emp)) |>
  mutate(age = factor(age, c("Y_LT14", "Y15-24", "Y25-54", "Y55-64", "Y_GE65"))) |>
  arrange(geo, time, sex, age) |>
  mutate(tact = emp/pop) |>
  group_by(geo, age, sex) |>
  arrange(time) |>
  fill(tact, .direction = "down") |>
  mutate(emp = ifelse(is.na(emp), pop*tact, emp)) |>
  group_by(geo, age) |>
  mutate(emp.alt = ifelse(time>="2024-01-01", pop*tact[sex=="M"], emp)) |>
  ungroup()

dep <- full |>
  group_by(geo, time) |>
  summarise(dep = (sum(pop)-sum(emp))/sum(emp),
            dep.alt = (sum(pop)-sum(emp.alt))/sum(emp.alt),
            .groups = "drop")

ggplot(full |> filter(time<="2030-01-01")) +
  aes(x=time, y=tact, color = age, linetype = sex) +
  geom_line() +
  scale_color_brewer(palette = "Blues") +
  facet_wrap(vars(geo), ncol = 2) +
  theme_ofce()
ggplot(dep |> filter(time<='2050-01-01')) +
  geom_line(aes(x=time, y=dep, color = geo)) +
  geom_line(aes(x=time, y=dep.alt, color = geo), linetype="dotted") +
  theme_ofce() +
  facet_wrap(vars(geo), ncol = 2) +
  scale_color_pays(format = "eurostat") +
  ofce_caption(
    source = "Eurostat demo_pjan pour la population, proj_23np pour les projections de population (baseline), lfsi_emp_a pour l'emploi par age et sexe",
    note = "La ratio de dépendance est le ratio entre les inactifs ou chômmeurs, de 0 à 100 ans rapporté aux actifs de 15 à 64 ans. Le trait pointillé indique le ratio de dépendance si le taux d'emploi des femmes est égale à celui des hommes.")
