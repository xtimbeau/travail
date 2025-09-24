library(tidyverse)

nace  <- tribble( ~a20, ~a10, ~marchand, ~hors_imm, ~hifi, ~hfi,  ~hi,
                 "A", "A",   TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "B", "B-E", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "C", "B-E", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "D", "B-E", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "E", "B-E", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "F", "F",   TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "G", "G-I", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "H", "G-I", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "I", "G-I", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "J", "J",   TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "K", "K",   TRUE,       TRUE,      FALSE, FALSE, TRUE,
                 "L", "L",   TRUE,       FALSE,     FALSE, TRUE,  FALSE,
                 "M", "M_N", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "N", "M_N", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "O", "O-Q", FALSE,      TRUE,      TRUE,  TRUE,  TRUE,
                 "P", "O-Q", FALSE,      TRUE,      TRUE,  TRUE,  TRUE,
                 "Q", "O-Q", FALSE,      TRUE,      TRUE,  TRUE,  TRUE,
                 "R", "R-U", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "S", "R-U", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "T", "R-U", TRUE,       TRUE,      TRUE,  TRUE,  TRUE,
                 "U", "R-U", TRUE,       TRUE,      TRUE,  TRUE,  TRUE ) |>
  mutate(l20 = label_eurostat(a20, dic="nace_r2"),
         l10 = label_eurostat(a10, dic="nace_r2"))

marchand <- nace |> filter(hors_imm&marchand) |> pull(a20)
marchand2 <- unique(c(nace |> filter(hors_imm&marchand) |> pull(a10)))

return(list(marchand = marchand, marchand2 = marchand2, nace = nace))
