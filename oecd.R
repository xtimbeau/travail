library(OECD)
library(tidyverse)
library(glue)
sna <- "OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA,1.1"
eo <- "OECD.ECO.MAD,DSD_EO@DF_EO,1.3"
query <- "Q..FRA.........."
pays <- "USA+FRA+DEU+GBR+ITA+ESP+NLD+BEL+JPN"
fr <-  OECD::get_dataset(sna, query)

b1g <-  OECD::get_dataset(sna, "Q..{pays}...B1G.._T...V..T0101" |> glue())
b1gq <-  OECD::get_dataset(sna, "Q..{pays}...B1GQ.....V.." |> glue())
p51c <- OECD::get_dataset(sna, "Q..{pays}...P51C.....V..T0103" |> glue())
sal <- OECD::get_dataset(dataset, "Q ..{pays}...SAL.._T.....T0110" |> glue())
self <- OECD::get_dataset(dataset, "Q..{pays}...SELF.._T.....T0110" |> glue())
d1 <- OECD::get_dataset(dataset, "Q..{pays}...D1.._T.....T0103" |> glue())

p51c_eo <- OECD::get_dataset(eo, "{pays}.PITISK+KTPV+RSCRP.A" |> glue())

self_eo <- OECD::get_dataset(eo, "{pays}.EG+POP+ES.Q" |> glue())

map(list(b1g, b1gq, p51c, sal, self, d1),
    ~.x |>
      mutate(ADJUSTMENT = factor(ADJUSTMENT, c("Y", "N"))) |>
      arrange(ADJUSTMENT) |>
      group_by(across(-c(ADJUSTMENT, ObsValue))) |>
      summarize(across(c(ADJUSTMENT, ObsValue), first)) |>
      group_by(REF_AREA) |>
      summarize(n=n(), nt = n_distinct(TIME_PERIOD), mt = max(TIME_PERIOD), adj = n_distinct(ADJUSTMENT)))
