library(tidyverse)

ror <- readr::read_csv2("data/metadata/BBSR_ROR.csv",
                        locale = locale(encoding = "latin1"))

names(ror) <- c("AGS",
                "Kreis_Name",
                "ROR_ID",
                "ROR")

ror <- ror %>% 
  mutate(ROR_ID = as.character(ROR_ID)) %>% 
  mutate(AGS = substr(stringi::stri_pad_left(AGS, width = 8, pad = "0"), 1, 5))

saveRDS(ror, file = "data/metadata/BBSR_ROR.rds")

