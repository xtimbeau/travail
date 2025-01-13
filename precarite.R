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

ggplot(prec) +
  aes(x = time, y = values, color = geo, group = geo) +
  geom_line(data = ~ rename(.x, geo2 = geo), aes(group = geo2),
            color = "gray80", linewidth = 0.2) +
  geom_line(show.legend = FALSE) +
  scale_color_pays(format = "eurostat") +
  facet_wrap(vars(geo), labeller = as_labeller(label_pays), ncol = 2) +
  scale_ofce_date() +
  theme_ofce()

cho <- get_eurostat("une_rt_q",
                    filters = list(
                      age = c("Y20-64", "Y15-24"),
                      geo = pays2,
                      sex = "T",
                      unit = "PC_POP",
                      s_adj = "SA")) |>
  drop_na() |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")),
         age = str_remove(age, "-")) |>
  pivot_wider(names_from = age, values_from = values)

ggplot(cho |> filter(time>="2010-01-01")) +
  aes(x = time, color = geo, group = geo) +
  geom_line(aes(y = Y2064), show.legend = FALSE) +
  geom_line(aes(y = Y1524), linetype = "dotted", show.legend = FALSE) +
  scale_color_pays(format = "eurostat") +
  facet_wrap(vars(geo), labeller = as_labeller(label_pays), ncol = 2) +
  scale_y_continuous(limits = c(0,10), oob = scales::squish) +
  scale_ofce_date() +
  theme_ofce()



