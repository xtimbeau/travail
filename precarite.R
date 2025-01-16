library(tidyverse)
library(eurostat)
library(ofce)

pays <- c("DE", "FR", "IT", "ES", "NL", "BE", "IE", "AT", "FI", "PT", "EL", "SK", "LU", "LT", "HR", "SI", "LV", "EE", "CY", "MT", "EA20")
pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

prec <- get_eurostat("lfsa_qoe_4ax1r2",
                     filters = list(nace_r2 = "TOTAL",
                                    geo = pays2, sex = "T",
                                    age = "Y20-64")) |>
  drop_na() |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))

prec_sum <- prec |>
  group_by(geo, age) |>
  summarize(
    last = values[time==max(time, na.rm=TRUE)],
    min = min(values, na.rm=TRUE),
    min_date = last(time[values==min(values, na.rm=TRUE)]),
    max = max(values, na.rm=TRUE),
    max_date = first(time[values==max(values, na.rm=TRUE)])) |>
  mutate( what = "3mcontract", age = "Y2064")

cho <- get_eurostat("une_rt_q",
                    filters = list(
                      age = c("Y20-64", "Y15-24"),
                      geo = pays2,
                      sex = "T",
                      unit = "PC_POP",
                      s_adj = "SA")) |>
  drop_na() |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")),
         age = str_remove(age, "-"))

cho_sum <- cho |>
  group_by(geo, age) |>
  summarize(
    last = values[time==max(time, na.rm=TRUE)],
    min = min(values, na.rm=TRUE),
    min_date = last(time[values==min(values, na.rm=TRUE)]),
    max = max(values, na.rm=TRUE),
    max_date = first(time[values==max(values, na.rm=TRUE)])) |>
  mutate( what = "chômage")

emp <- get_eurostat("lfsa_egan",
                    filters = list(citizen = "TOTAL",
                                   geo = pays2, sex = "T",
                                   age = "Y20-64")) |>
  select(geo, time, emp = values)

secondjob <- get_eurostat("lfsa_e2ged",
                          filters = list(isced11 = "TOTAL",
                                         geo = pays2, sex = "T",
                                         age = "Y20-64")) |>
  select(geo, time, values) |>
  left_join(emp, by = c("geo", "time")) |>
  mutate(values = values/emp) |>
  drop_na(values) |>
  group_by(geo) |>
  summarize(
    last = values[time==max(time, na.rm=TRUE)],
    min = min(values, na.rm=TRUE),
    min_date = last(time[values==min(values, na.rm=TRUE)]),
    max = max(values, na.rm=TRUE),
    max_date = first(time[values==max(values, na.rm=TRUE)])) |>
  mutate(what = "second job", age = "Y2064")

newjob <- get_eurostat("lfsa_enewasn",
                       filters = list(wstatus = "EMP",
                                      citizen = "TOTAL",
                                      geo = pays2,
                                      age = "Y20-64")) |>
  group_by(geo) |>
  summarize(
    last = values[time==max(time, na.rm=TRUE)],
    min = min(values, na.rm=TRUE),
    min_date = last(time[values==min(values, na.rm=TRUE)]),
    max = max(values, na.rm=TRUE),
    max_date = first(time[values==max(values, na.rm=TRUE)])) |>
  mutate(what = "newjob", age = "Y2064")

summary <- bind_rows(prec_sum, cho_sum, secondjob, newjob)

return(list(cho = cho, prec = prec, sum = summary))
