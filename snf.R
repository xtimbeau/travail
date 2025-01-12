library(tidyverse)
library(eurostat)
library(ofce)

pays <- c("DE", "FR", "IT", "ES", "NL", "BE", "IE", "AT", "FI", "PT", "EL", "SK", "LU", "LT", "HR", "SI", "LV", "EE", "CY", "MT", "EA20")
pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

nasa.raw <- get_eurostat("nasa_10_nf_tr",
                         filters = list(na_item = c("B1G", "P51C", "B2A3N", "D51", "B1N", "D29", "D39"),
                                        geo = pays, unit = "CP_MEUR",
                                        sector=c("S11", "S1001"))) |>
  drop_na()

nasa <- nasa.raw |>
  pivot_wider(names_from = c(na_item, direct), values_from = values) |>
  mutate(
    van = B1G_PAID - P51C_PAID,
    tp = (B2A3N_PAID-D51_PAID)/van,
    tis = (D51_PAID + D29_PAID - D39_RECV)/van,
  ) |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))

ggplot(nasa |> filter(geo %in% pays2, time >= "1995-01-01")) +
  aes(x = time, y = tp, color = geo, group = geo) +
  geom_line(data = ~ rename(.x, geo2 = geo), aes(group = geo2),
            color = "gray80", linewidth = 0.2) +
  geom_ribbon(aes(ymin= tp, ymax = tp+tis, fill = geo),
              color = "transparent", alpha= 0.2, show.legend = FALSE ) +
  geom_line(show.legend = FALSE) +
  scale_color_pays(format = "eurostat") +
  facet_wrap(vars(geo), labeller = as_labeller(label_pays), ncol = 2) +
  scale_ofce_date() +
  theme_ofce()
