library(tidyverse)

nace <- tribble( ~a20, ~a10, ~marchand, ~hors_imm,
                 "A", "A", TRUE, TRUE,
                 "B", "B-E", TRUE, TRUE,
                 "C", "B-E", TRUE, TRUE,
                 "D", "B-E", TRUE, TRUE,
                 "E", "B-E", TRUE, TRUE,
                 "F", "F", TRUE, TRUE,
                 "G", "G-I", TRUE, TRUE,
                 "H", "G-I", TRUE, TRUE,
                 "I", "G-I", TRUE, TRUE,
                 "J", "J", TRUE, TRUE,
                 "K", "K", TRUE, TRUE,
                 "L", "L", TRUE, FALSE,
                 "M", "M_N", TRUE, TRUE,
                 "N", "M_N", TRUE, TRUE,
                 "O", "O-Q", FALSE, TRUE,
                 "P", "O-Q", FALSE, TRUE,
                 "Q", "O-Q", FALSE, TRUE,
                 "R", "R-U", TRUE, TRUE,
                 "S", "R-U", TRUE, TRUE,
                 "T", "R-U", TRUE, TRUE,
                 "U", "R-U", TRUE, TRUE ) |>
  mutate(l20 = label_eurostat(a20, dic="nace_r2"),
         l10 = label_eurostat(a10, dic="nace_r2"))

marchand <- nace |> filter(hors_imm) |> pull(a20)
marchand2 <- unique(c(nace |> filter(hors_imm) |> pull(a10)))

return(list(marchand = marchand, marchand2 = marchand2, nace = nace))
