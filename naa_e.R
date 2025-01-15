library(tidyverse)
library(eurostat)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")

naa <- get_eurostat("nama_10_a64",
                    filters = list(
                      nace_r2 = marchand,
                      na_item = "D1",
                      geo = pays2,
                      unit = "CP_MEUR") ) |>
  drop_na() |>
  rename(D1 = values)

naa_e <- get_eurostat("nama_10_a64_e",
                      filters = list(nace_r2  = marchand, geo = pays2,
                                     unit = c("THS_PER"),
                                     na_item = c("SAL_DC", "SELF_DC")) ) |>
  drop_na(values) |>
  select(nace_r2, geo, time, values, na_item) |>
  pivot_wider(names_from = na_item, values_from = values) |>
  left_join(naa, by = c("geo", "time", "nace_r2")) |>
  filter(nace_r2 != "A") |>
  group_by(time, geo) |>
  summarize(sal = sum(D1, na.rm = TRUE),
            self = sum(D1/SAL_DC*SELF_DC, na.rm = TRUE),
            .groups = "drop") |>
  transmute(time, geo,
            tsal = 1+self/sal)

return(naa_e)
